
list.of.packages <- c("GetoptLong", "magrittr", "curl", "jsonlite", "rjson", "purrr", "data.table", 
                      "gplots", "sp", "rgdal", "geosphere", "dplyr", "ggmap", "stats", "igraph", 
                      "cluster", "factoextra", "magrittr", "fpc", "ggplot2", "caret", "mlbench", 
                      "skimr", "e1071", "rpart", "rpart.plot", "FactoMineR", "corrplot", "RJDBC", 
                      "rJava", "ppclust", "kmodR", "rlist", "ggsn", "gridExtra", "future.apply",
                      "gghighlight", "ggdendro")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(GetoptLong)
library(magrittr)
library(curl)
library(jsonlite)
library(rjson)
library(purrr)
library(data.table)
library(gplots)
library(sp)
library(rgdal)
library(geosphere)
library(dplyr)
library(ggmap)
library(stats)
library(igraph)
library(cluster)
library(factoextra)
library(magrittr)
library(fpc)
library(ggplot2)
library(caret)
library(mlbench)
library(skimr)
library(e1071)
library(rpart)
library(rpart.plot)
library(FactoMineR)
library(corrplot)
library(RJDBC)
library(rJava)
library(ppclust)
library(kmodR)
library(rlist)
library(gridExtra)
library(future.apply)

# call the credentials script
source("credentials.R")

## using snowflake to speed up data cleaning and preprocessing (R and python are slow for this)
sfConn = function(role = "sysadmin") {
  jdbcDriver = RJDBC::JDBC(driverClass = "com.snowflake.client.jdbc.SnowflakeDriver", classPath = "~/tmp/snowflake-jdbc-3.12.4.jar")
  
  RJDBC::dbConnect(jdbcDriver, GetoptLong::qq("jdbc:snowflake://ke72837.eu-central-1.snowflakecomputing.com:443/?account=KE72837&warehouse=compute_wh&role=@{role}"),
                   "mattia", snowflakePasswordStoredInCredentialsFile)
}
sfSql = function(query, role = "sysadmin") {
  conn = sfConn(role)
  tryCatch({
    result = DBI::dbGetQuery(conn, query)
    result
  }, error = function(e) {
    stop(e)
  }, finally = {
    DBI::dbDisconnect(conn)
  })
}

# get distance in meters, kms ...
get_geo_distance = function(long1, lat1, long2, lat2, units = "km") {
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  distance
}

# plot google maps
finplot = function( gmap, d, title, alpha=0.6, bins=100, heatmap=0) {
  xf = d[, 1]
  yf = d[, 2]
  
  fin_map = ggmap(gmap)
  if ( heatmap == 1 ) {
    fin_map = fin_map + 
      stat_summary_2d(data = d, aes(x=xf, y=yf, z=rep(1,length(xf))), fun = sum, alpha = alpha, bins = bins) + 
      scale_fill_gradient(name = "Data points count", low = "blue", high = "red") 
  }
  else {
    fin_map = fin_map + geom_point(aes(xf, yf), data = d, colour = "blue") 
  }
  
  fin_map = fin_map + 
    ggtitle(title) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=10),
          axis.text.y = element_text(face="bold", color="#993333", size=10),
          panel.grid.major = element_line(colour="black", size=0.5),
          panel.grid.minor = element_line(colour="black", size=0.5)
    )
  
  fin_map
}

#momentarly unsued
simplePlotSinglePath = function( singlePathsContainer, i ) {
  colfunc <- colorRampPalette(c("green", "black", "red"))
  bsn = paste("run: ", c(i))
  timeframe = paste( min(singlePathsContainer[[i]]$DATETIME), paste(" - ", max(singlePathsContainer[[i]]$DATETIME)) )
  mainTitle = GetoptLong::qq("@{bsn} :: @{timeframe}")
  mainTitle
  par(mfrow=c(3,1))
  plot(singlePathsContainer[[i]]$GPSLONGITUDE,
       singlePathsContainer[[i]]$GPSLATITUDE,
       main=mainTitle,
       col=colfunc(length(singlePathsContainer[[i]]$GPSLATITUDE)) )
  plot(singlePathsContainer[[i]]$FUELCONSUMPTION_L_H, main="Fuel consumption")
  abline(h = 0, lty = 2)
  abline(h = 30, lty = 2, col="red")
  plot(singlePathsContainer[[i]]$SPEEDGEARBOX_KM_H, main="Tractor speed")
  abline(h = 0, lty = 2)
  abline(h = 20, lty = 2, col="red")
}

# proper plotting
plotSinglePath = function( singlePath, serial ) {
  
  # palette used to differentiate the start of the path from the end
  colfunc <- colorRampPalette(c("cyan", "blue"))
  
  # get the time frame of the path (start - end)
  timeframe = paste( min(singlePath$DATETIME), paste(" - ", max(singlePath$DATETIME)) )
  mainTitle = GetoptLong::qq("Tractor: @{serial}\nloc. start (in cyan) - loc. finish (in blue) :: @{timeframe}")
  lgnd_pos_x = .90
  lgnd_pos_y = .85
  
  # coordinates
  t = singlePath$DATETIME
  x = singlePath$GPSLONGITUDE
  y = singlePath$GPSLATITUDE
  
  oneMileInDegLat = 0.00140972
  oneMileInDegLon = 0.0216916
  
  limit = list(
    left = min(x)-oneMileInDegLon, 
    bottom = min(y)-oneMileInDegLon, 
    right = max(x)+oneMileInDegLon, 
    top = max(y)+oneMileInDegLon
  )
  
  # now, check how many field cluster centers are near this path
  centersInsideTheLimitsList = list(x=vector(), y=vector())
  count = 0
  for (l in 1:nrow(fields_centers)) {
    xc = fields_centers[l,"x"]
    yc = fields_centers[l,"y"]
    
    if ( xc > limit$left & xc < limit$right & yc < limit$top & yc > limit$bottom ) {
      centersInsideTheLimitsList$x = append(centersInsideTheLimitsList$x, xc)
      centersInsideTheLimitsList$y = append(centersInsideTheLimitsList$y, yc)
      count = count + 1
    }  
  }
  centersInsideTheLimits = as.data.frame(cbind(as.vector(centersInsideTheLimitsList$x), as.vector(centersInsideTheLimitsList$y)) )
  colnames(centersInsideTheLimits) = c("x", "y")
  
  ## plots
  
  # path on the map first (with cluster centers)
  g1 = ggplot() +
    geom_point( aes(as.vector(x), as.vector(y)), data=as.data.frame(cbind(x,y)), colour=colfunc(length(x)), size = 4 ) +
    geom_point( aes(centersInsideTheLimits$x, centersInsideTheLimits$y), data=centersInsideTheLimits, colour="green", size = 4 ) +
    ggtitle(mainTitle) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=10),
          axis.text.y = element_text(face="bold", color="#993333", size=10),
          panel.grid.major = element_line(colour="white", size=0.5),
          panel.grid.minor = element_line(colour="white", size=0.5)
    )
  
  y = singlePath$FUELCONSUMPTION_L_H
  y_ma_5 = singlePath$FUEL_MA_5M
  y_ma_10 = singlePath$FUEL_MA_10M
  y_ma_20 = singlePath$FUEL_MA_20M
  y_ma_30 = singlePath$FUEL_MA_30M
  
  # fuel consumption
  g2 = ggplot() +
    geom_point( aes(1:length(y), as.vector(y), colour = "FC 1min sample"), data=as.data.frame(cbind(1:length(y),y)), size = 4 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_5), colour = "FC 5min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_5)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_10), colour = "FC 5min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_10)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_20), colour = "FC 10min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_20)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_30), colour = "FC 20min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_30)), size = 2 ) +
    ggtitle("Fuel consumption") +
    xlab("Time (index)") +
    ylab("Fuel consumption (l/hr)") +
    scale_colour_manual(name = "Legend", 
                        breaks = c("FC 1min sample", "FC 5min moving avg", "FC 5min moving avg", "FC 10min moving avg", "FC 20min moving avg"),
                        values=c("grey", "#264120", "#669054", "#9BC883", "#D2F7B8")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=14),
          axis.text.y = element_text(face="bold", color="#993333", size=14),
          legend.position = c(lgnd_pos_x, lgnd_pos_y)
    )
  
  # speed
  y = singlePath$SPEEDGEARBOX_KM_H
  y_ma_5 = singlePath$SPEED_MA_5M
  y_ma_10 = singlePath$SPEED_MA_10M
  y_ma_20 = singlePath$SPEED_MA_20M
  y_ma_30 = singlePath$SPEED_MA_30M
  flag_sp = singlePath$ISINTHEFIELD * mean(y)
  
  g3 = ggplot() +
    geom_point( aes(1:length(y), as.vector(y), colour = "Speed 1min sample"), data=as.data.frame(cbind(1:length(y),y)), size = 4 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_5), colour = "Speed 5min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_5)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_10), colour = "Speed 5min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_10)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_20), colour = "Speed 10min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_20)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_30), colour = "Speed 20min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_30)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(flag_sp), colour="FieldFlag"), data=as.data.frame(cbind(1:length(y),flag_sp)), size = 2 ) +
    ggtitle("Speed") +
    xlab("Time (index)") +
    ylab("Speed (km/h)") +
    scale_colour_manual(name = "Legend", 
                        breaks = c("Speed 1min sample", "Speed 5min moving avg", "Speed 5min moving avg", "Speed 10min moving avg", "Speed 20min moving avg", "FieldFlag" ),
                        values=c("grey", "#264120", "#669054", "#9BC883", "#D2F7B8", "orange")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=14),
          axis.text.y = element_text(face="bold", color="#993333", size=14),
          legend.position = c(lgnd_pos_x, lgnd_pos_y)
    )
  
  # load
  y = singlePath$ENGINELOAD
  y_ma_5 = singlePath$ENGINELOAD_MA_5M
  y_ma_10 = singlePath$ENGINELOAD_MA_10M
  y_ma_20 = singlePath$ENGINELOAD_MA_20M
  y_ma_30 = singlePath$ENGINELOAD_MA_30M
  
  g4 = ggplot() +
    geom_point( aes(1:length(y), as.vector(y), colour = "Load 1min sample"), data=as.data.frame(cbind(1:length(y),y)), size = 4 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_5), colour = "Load 5min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_5)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_10), colour = "Load 5min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_10)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_20), colour = "Load 10min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_20)), size = 2 ) +
    geom_line( aes(1:length(y), as.vector(y_ma_30), colour = "Load 20min moving avg"), data=as.data.frame(cbind(1:length(y),y_ma_30)), size = 2 ) +
    ggtitle("Engine load") +
    xlab("Time (index)") +
    ylab("Engine load (%)") +
    scale_colour_manual(name = "Legend", 
                        breaks = c("Load 1min sample", "Load 5min moving avg", "Load 5min moving avg", "Load 10min moving avg", "Load 20min moving avg" ),
                        values=c("grey", "#264120", "#669054", "#9BC883", "#D2F7B8")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=14),
          axis.text.y = element_text(face="bold", color="#993333", size=14),
          legend.position = c(lgnd_pos_x, lgnd_pos_y)
    )
  
  grid.arrange(g1,g2,g3,g4,ncol=2)
  
  # engine rpms and PTOs
  rpms = singlePath$ENGINE_RPM
  ptof = singlePath$PTOFRONT_RPM
  ptor = singlePath$PTOREAR_RPM
  pb = singlePath$PARKINGBREAKSTATUS * mean(rpms)/10
  gs = singlePath$GEARSHIFT
  difflock = singlePath$DIFFERENTIALLOCKSTATUS
  isInField = singlePath$ISINTHEFIELD
  
  g1 = ggplot() +
    geom_point( aes(1:length(rpms), as.vector(rpms), colour = "Engine RPMs at 1min sample"), data=as.data.frame(cbind(1:length(rpms),rpms)), size = 2 ) +
    geom_point( aes(1:length(ptof), as.vector(ptof), colour = "PTO front RPMs at 1min sample"), data=as.data.frame(cbind(1:length(ptof),ptof)), size = 2 ) +
    geom_point( aes(1:length(ptor), as.vector(ptor), colour = "PTO rear RPMs at 1min sample"), data=as.data.frame(cbind(1:length(ptor),ptor)), size = 2 ) +
    geom_point( aes(1:length(pb), as.vector(pb), colour = "Parking break at 1min sample"), data=as.data.frame(cbind(1:length(pb),pb)), size = 2 ) +
    ggtitle("Engine RPMs") +
    xlab("Time (index)") +
    ylab("Engine RPMs") +
    scale_colour_manual(name = "Legend", 
                        breaks = c("Engine RPMs at 1min sample", "PTO front RPMs at 1min sample", "PTO rear RPMs at 1min sample", "Parking break at 1min sample" ),
                        values=c("#264120", "#9BC883", "#D2F7B8", "grey")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=14),
          axis.text.y = element_text(face="bold", color="#993333", size=14)
    )
  
  g2 = ggplot() +
    geom_point( aes(1:length(gs), as.vector(gs), colour = "Gear shifts at 1min sample"), data=as.data.frame(cbind(1:length(gs),gs)), size = 2 ) +
    geom_point( aes(1:length(difflock), as.vector(difflock), colour = "Diff locks at 1min sample"), data=as.data.frame(cbind(1:length(difflock),difflock)), size = 2 ) +
    geom_line( aes(1:length(isInField), as.vector(isInField), colour = "Field flag"), data=as.data.frame(cbind(1:length(isInField),isInField)), size = 1 ) +
    ggtitle("Shifts & DiffLocks") +
    xlab("Time (index)") +
    ylab("Shifts & DiffLocks") +
    scale_colour_manual(name = "Legend",
                        breaks = c("Gear shifts at 1min sample", "Diff locks at 1min sample", "Field flag" ),
                        values=c("#264120", "grey", "orange")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=14),
          axis.text.y = element_text(face="bold", color="#993333", size=14)
    )
  
  grid.arrange(g1,g2,ncol=2)
} 

# get a single run of a tractor (from start to end) => end is here detected as a rest period of at least "idleTimeInMinutes"
getWorkingSessions = function( selectedTractor, idleTimeInMinutes ) {
  df = sfSql(GetoptLong::qq('
      SELECT 
          *
          , lead(dateTime) over (partition by serialNumber order by dateTime) AS nextRow
          , ABS(TIMEDIFF(minute, dateTime, nextRow)) timedifference
          , COALESCE( IFF(timeDifference <= @{idleTimeInMinutes}, 1, 2), 1) symb
      FROM temp.public.allData_1minSampling
      WHERE serialNumber = \'@{selectedTractor}\'
      ORDER BY dateTime
      ')
  )
  df
}

## hide this before sharing the R file
register_google(key=googleApiKeyStoredInCredentialsFile)



## ===========================================================================
# get the prepared data in snowflake (discard speedRadar_km_h and CreeperStatus => wrong data in them)
dataOrdered = sfSql("
  SELECT 
      datetime
      , serialNumber
      , gpsLongitude
      , gpsLatitude
      , totalWorkingHours
      , Engine_rpm
      , EngineLoad
      , FuelConsumption_l_h
      , SpeedGearbox_km_h
      , TempCoolant_C
      , PtoFront_rpm
      , PtoRear_rpm
      , GearShift
      , TempAmbient_C
      , ParkingBreakStatus
      , DifferentialLockStatus
      , AllWheelDriveStatus
  FROM temp.public.allData 
  ORDER BY dateTime
")

# get some summary on the attributes
summary(dataOrdered)

# get some correlation between attributes (see if there are some dependencies)
nonNaDF = subset(dataOrdered, select=-c(DATETIME,SERIALNUMBER,ALLWHEELDRIVESTATUS))
nonNaDF[is.na(nonNaDF)] = 0
correlationMatrix = cor(nonNaDF)
corrplot(correlationMatrix) # <= this one makes sense

# raw data
x = dataOrdered$GPSLONGITUDE
y = dataOrdered$GPSLATITUDE
d = as.data.frame( cbind(x,y) )

# map the whole thing
g = ggplot(d, aes(x=x, y=y) ) +
  geom_bin2d(bins = 80) +
  scale_fill_continuous(type = "viridis") +
  theme_grey() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="#993333", size=14),
        axis.text.y = element_text(face="bold", color="#993333", size=14),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)
  )
plot(g)

# sampled each 10 minutes
dataOrderedSampled10m = sfSql("SELECT * FROM temp.public.allData_10minSampling")

# raw data for 10m samples
x_sampled10m = dataOrderedSampled10m$GPSLONGITUDE
y_sampled10m = dataOrderedSampled10m$GPSLATITUDE
d_sampled10m = as.data.frame( cbind(x_sampled10m,y_sampled10m) )

# sample as well as to 1minute
dataOrderedSampled1m = sfSql("SELECT * FROM temp.public.allData_1minSampling")

# raw data for 1m samples
x_sampled1m = dataOrderedSampled1m$GPSLONGITUDE
y_sampled1m = dataOrderedSampled1m$GPSLATITUDE
d_sampled1m = as.data.frame( cbind(x_sampled1m,y_sampled1m) )


# init google maps
ggmap_offset = 0.2
ggmap_offset_local = 0.05

# compute the min/max (x,y) ranges
global_ranges = list(
    left = min(dataOrdered$GPSLONGITUDE)-ggmap_offset, 
    bottom = min(dataOrdered$GPSLATITUDE)-ggmap_offset, 
    right = max(dataOrdered$GPSLONGITUDE)+ggmap_offset, 
    top = max(dataOrdered$GPSLATITUDE)+ggmap_offset
) 

# get the max and min latitudes as characters (for ggmap use)
sf <- c(left = global_ranges$left, bottom = global_ranges$bottom, right = global_ranges$right, top = global_ranges$top ) # options2: provide the center point as c(lon = ..., lat = ...)

# get the google map for the specific location
global_map <- get_map(location = sf, scale = 1)

# plot the points on the map
g = finplot(global_map, d_sampled10m, "Premiki mehanizacije po lokacijah", alpha=0.6, bins=100, heatmap=0)
plot(g)

# plot the 2D histogram instead (the heatmap)
g = finplot(global_map, d, "Premiki mehanizacije po lokacijah", alpha=0.6, bins=100, heatmap=1)
plot(g)

# create a bin grid with bin size (1km x 1km) - 100 hectar grid (1 big field ~ 100 hectar)
bins_y_global = ceiling( (global_ranges$top-global_ranges$bottom)/(1/60/2) )
bins_x_global = ceiling( (global_ranges$right - global_ranges$left)/(1/60/2) )

# plot the histogram (see how many locations are the most dense)
h_global = hist2d(x, y, nbins=c(bins_x_global,bins_y_global), 
                  same.scale=FALSE, 
                  col=c("grey", heat.colors(40))
                 )
# the tractors needs to stay inside a bin at least 5 hr to make the location a candidate for a field
veryApproximativeNumberOfFields = length( h_global$counts[ h_global$counts > 360*5 ] ) #~300

## we can observe that the tractor are working on 3 main "zones" (see the red spots on the heatmap)
## hence if we are interested in discovering where exactly the centers of the fields are, we just need
## to look only onto these 3 zones
limits1 = list(left=20.0, bottom=45.95, right=20.35, top=46.15)
limits2 = list(left=20.1, bottom=45.55, right=20.45, top=45.70)
limits3 = list(left=20.0, bottom=45.2, right=20.5, top=45.55)
sf1 <- c(left = limits1$left-ggmap_offset_local, bottom = limits1$bottom-ggmap_offset_local, right = limits1$right+ggmap_offset_local, top = limits1$top+ggmap_offset_local)
sf2 <- c(left = limits2$left-ggmap_offset_local, bottom = limits2$bottom-ggmap_offset_local, right = limits2$right+ggmap_offset_local, top = limits2$top+ggmap_offset_local)
sf3 <- c(left = limits3$left-ggmap_offset_local, bottom = limits3$bottom-ggmap_offset_local, right = limits3$right+ggmap_offset_local, top = limits2$top+ggmap_offset_local)

# get sattelite maps (don't run this too many times cause you will run out of google cloud credits)
map1s <- get_map(location = sf1, scale = 1, maptype="satellite")
map2s <- get_map(location = sf2, scale = 1, maptype="satellite")
map3s <- get_map(location = sf3, scale = 1, maptype="satellite")

# simple terrain
map1 <- get_map(location = sf1, scale = 1 )
map2 <- get_map(location = sf2, scale = 1 )
map3 <- get_map(location = sf3, scale = 1 )

limits = list(limits1, limits2, limits3)
maps = list(map1s, map2s, map3s)




## ===========================================================================
# get all the centres of the fields
fields_centers = data.frame()

# set some holders to store the plots and other important stuff for the report
zonesHistograms = list()
elbowPlots = list()
heatmapsClusters = list()

# loop over the 3 different zones
for ( l in 1:length(limits) ) {
  lim = limits[[l]]
  map = maps[[l]]
  
  dataFiltered = dataOrderedSampled1m %>%
    #filter(SERIALNUMBER == distinctSerials[1]) %>%
    filter( GPSLONGITUDE >= lim$left &
              GPSLONGITUDE <= lim$right &
              GPSLATITUDE >= lim$bottom &
              GPSLATITUDE <= lim$top) %>%
    arrange(DATETIME)
  
  # let's see if this is a good way to detect the fields
  xp = (dataFiltered %>% filter(FUEL_MA_10M > 30))$GPSLONGITUDE
  yp = (dataFiltered %>% filter(FUEL_MA_10M > 30))$GPSLATITUDE
  dp = as.data.frame( cbind(xp,yp) )
  
  xn = dataFiltered$GPSLONGITUDE
  yn = dataFiltered$GPSLATITUDE
  dn = as.data.frame( cbind(xn,yn) )
  
  # sampling even further (take each 10th coordinate)
  #dp = dp[seq(1, nrow(dp), 10), ]
  dn = dn[seq(1, nrow(dn), 30), ]
  colnames(dn) = c("long", "lat") # requirement og ggsn
  
  # analyze the clusters over all the locations (including streets)

  # first create ~400x400m bins
  bins_y = (lim$top-lim$bottom)/(1/240)
  bins_x = (lim$right - lim$left)/(1/240)
  
  # distance heatmap
  h=hist2d(xn, yn, nbins=c(bins_x,bins_y), same.scale=FALSE, col=c("white", heat.colors(20)))
  zonesHistograms[[l]] = h
  # dph = as.data.frame( cbind(h$x, h$y) )
  # distance <- get_dist(dph)
  # fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  
  set.seed( as.integer(as.POSIXct( Sys.time() )) ) # get epoch time to seed the PRNG
  maxClusters = 18 # performance issues contraint
  
  # find the elbow curves (for both data sets)
  nbws = fviz_nbclust(dn, kmeans, method = "wss", k.max = maxClusters) 
  #nbgs = fviz_nbclust(dn, kmeans, method = "gap_stat", nboot=20) # option 2
  #nbsl = fviz_nbclust(dn, kmeans, method = "silhouette") # option 3
  #nbwsDp = fviz_nbclust(dp, kmeans, method = "wss", k.max = maxClusters) 
  
  elbowPlots[[l]] = nbws
  plot(nbws)
  
  # get the optimal number of clusters for both the filtered 
  # (only where the consumption is high) and the unfiltered data
  k1 = which( nbws$data$y == min(nbws$data$y))
  #k2 = which( nbwsDp$data$y == min(nbwsDp$data$y))
  
  # best choice
  #k = min( k1, k2 )
  k = k1
  
  # clustering with kmeans dicarding outliers (WOW)
  clust = kmod(dp, k) # , ceiling(nrow(dp)*2/100) ~~ remove 2% of all outliers
  centers = as.data.frame( clust$C )
  outliers = as.data.frame( clust$L )
  
  # fuzzy clustering <- FANTASTIC also, but it's missing few points in the zone 1
  #cm <- cmeans(dp, 15, m=5, iter.max=200) 
  #cm <- cmeans(dp, k)
  
  g = finplot(map, dp, GetoptLong::qq("Premiki traktorjev po lokacijah znotraj cone @{l}"), alpha=0.6, bins=100, heatmap=1) +
    # geom_point(aes(as.vector(dn[,1]), as.vector(dn[,2])), data=as.data.frame(dn), colour="grey" ) +
    geom_point(aes(as.vector(centers[,1]), as.vector(centers[,2])), data=as.data.frame(centers), colour="green", size = 4 ) +
    # geom_point(aes(as.vector(cm$centers[,1]), as.vector(cm$centers[,2])), data=as.data.frame(cm$centers), colour="red", size=4 ) +
    ggsn::scalebar(dn, dist = 5, dist_unit = "km", transform = TRUE, model = "WGS84", location = "bottomright")
  plot(g)
  
  heatmapsClusters[[l]] = g
  
  fields_centers = as.data.frame( rbind(fields_centers, clust$C) ) #, cm$centers
}

plot( elbowPlots )
length(elbowPlots)

# check manually some cluster center
# get_geo_distance(20.181258,45.318750,20.351847,45.324694,'km')

rownames(fields_centers) = NULL
colnames(fields_centers) = c("x", "y")
#fields_centers

## ###########################################################################################
## ###########################################################################################
## ###########################################################################################
## ###########################################################################################

## add new column in the data frame (the slow way)
# distances_holder = matrix(rep(0,nrow(fields_centers)),ncol=1)
# classifier = matrix(rep(0,nrow(dataOrderedSampled1m)),ncol=1)
# 
# listOfPrintedValue = vector()
# # flag which event was registered on the field
# for ( row in 1:nrow(dataOrderedSampled1m) ) {
#   x1 = dataOrderedSampled1m[row, "GPSLONGITUDE"]
#   y1 = dataOrderedSampled1m[row, "GPSLATITUDE"]
# 
#   for ( j in 1:nrow(fields_centers)) {
#     x2 = fields_centers[j,1]
#     y2 = fields_centers[j,2]
# 
#     distances_holder[j] = get_geo_distance(x1,y1,x2,y2) # abs( sqrt( (x1-x2)^2 + (y1-y2)^2 ) )
#   }
# 
#   # if the position is far away less than 1km from the cluster centre
#   # then presume you are already on the field
#   if ( min(distances_holder) < 1 ) {
#     classifier[row] = 1
#   }
#   
#   percentage = ceiling(row/nrow(dataOrderedSampled1m)*100)
#   if (not(percentage %in% listOfPrintedValue) ) {
#     print(GetoptLong::qq("@{percentage}%"))
#     listOfPrintedValue = append( listOfPrintedValue, percentage )
#   }
# }


## add new column in the data frame (the fast way)
X = as.data.frame( rep(0,nrow(dataOrderedSampled1m)) )
colnames(X) = c("IsInTheField")
dataOrderedSampled1m$IsInTheField = X

# prepare the apply functions to get rid of for loops
getDistKm = function(x,y,...) {
  additional.args <- list(...)
  x1 <- additional.args[[1]]
  y1 <- additional.args[[2]]
  d = get_geo_distance(x1,y1,x,y)
  d
}

fillColumnIsInTheField <- function(x,y,...) {
  
  additional.args <- list(...)
  cx <- additional.args$cx 
  cy <- additional.args$cy
  distances_holder = mapply(getDistKm, cx, cy, MoreArgs = list(x,y) ) 
  # for ( j in 1:length(cx)) {
  #   x2 = cx[j]
  #   y2 = cy[j]
  # 
  #   distances_holder[j] = get_geo_distance(x,y,x2,y2) # abs( sqrt( (x1-x2)^2 + (y1-y2)^2 ) )
  # }
  
  classifier_value = 0
  if ( min(distances_holder) < 1.5 ) { 
    classifier_value = 1
  }
  classifier_value
}

# compute the distances to the fields centers (the most computing intensive step - can take up to 20 min)
plan(multiprocess)
dataOrderedSampled1m$IsInTheField <- future.apply::future_mapply(fillColumnIsInTheField,
                                                                 dataOrderedSampled1m$GPSLONGITUDE,
                                                                 dataOrderedSampled1m$GPSLATITUDE,
                                                                 MoreArgs = list( cx=fields_centers$x, cy=fields_centers$y )
                                                                )

# check how many events were recorded on the field and how many on the street
nrow(dataOrderedSampled1m)
nrow(dataOrderedSampled1m %>% filter(IsInTheField == 1))
nrow(dataOrderedSampled1m)-nrow(dataOrderedSampled1m %>% filter(IsInTheField == 1))


## ###########################################################################################
## ###########################################################################################
## ###########################################################################################
## ###########################################################################################

# get the distinct tractors serial numbers
distinctSerials = as.vector( pull( dataOrdered %>% distinct(SERIALNUMBER) ) )

## Time series analysis

# count the time transporting for each tractor
transportTimeList = as.vector(rep(0,length(distinctSerials)))
activityTimeList  = as.vector(rep(0,length(distinctSerials)))

singlePathsContainerList = list()

for ( tractorCounter in 1:length(distinctSerials) ) {
    
    serial = distinctSerials[tractorCounter]
    # combine with the data processed earlier
    # choose 2 hours as the lag parameter
    df_temp = getWorkingSessions(serial, 120)
    column = as.vector( (dataOrderedSampled1m %>% filter(SERIALNUMBER == serial) %>% arrange(DATETIME))$IsInTheField )
    column_df = as.data.frame( column )
    colnames(column_df) = c("ISINTHEFIELD")
    df = cbind( df_temp, column_df )
    
    if ( nrow(df) == 0 ) {
      print(GetoptLong::qq("Something's wrong with the query for the tractor @{serial}"))
      break
    }
    # now extract each tractor daily (or multi days) working path
    emptyVec = rep(0L, nrow(df))
    counter = 1
    
    for ( i in 1:nrow(df) ) {
      emptyVec[i] = counter
      if ( df$SYMB[i] == 2 ) {
        counter = counter + 1
      }
    }
    
    dfCombined = cbind(df,emptyVec)
    l = as.vector( pull( dfCombined %>% distinct(emptyVec) ) )
    # View(df[1:250,])
    
    # plot the histogram to see if there are some overnighters
    #hist(dfCombined$emptyVec, breaks=length(l))
    #max(dfCombined$emptyVec)

    # a lot of single paths are short and with no movement (hence discard them)
    singlePathsContainer = list()
    counter = 1
    for ( i in 1:max(dfCombined$emptyVec) ) {
      singlePath = dfCombined %>% filter(emptyVec == i) %>% arrange(DATETIME)
      if ( nrow(singlePath) >= 8 ) {
        singlePathsContainer[[counter]] = singlePath
        counter = counter + 1
      }
    }
    
    singlePathsContainerList[[tractorCounter]] = singlePathsContainer
    
    for ( i in 1:length(singlePathsContainer) ) {
      singleRun = singlePathsContainer[[i]] 
      totalTime = sum(singleRun$SYMB)-1
      activityTime = sum((singleRun %>% filter(singleRun$ISINTHEFIELD == 1))$SYMB)-1
      transportTime = totalTime - activityTime
      
      transportTimeList[tractorCounter] = transportTimeList[tractorCounter] + transportTime
      activityTimeList[tractorCounter] = activityTimeList[tractorCounter] + activityTime
    }
}

## ###########################################################################################
## ###########################################################################################
## ###########################################################################################
## ###########################################################################################

## ====================================================
# Question 1)

tt = sum(transportTimeList)/60
at = sum(activityTimeList)/60
ct = at + tt
tt/ct*100
at/ct*100
tt
at
ct

totals = as.vector( as.numeric(round((transportTimeList + activityTimeList)/60,2)) )
totals
## compute the error we made comparing to the totalWorkingHours field (SUM)
df = data.frame( distinctSerials, totals )
colnames(df) = c("SERIALNUMBER", "TOTAL")
df = df %>% arrange(SERIALNUMBER)
fromSnowflake = sfSql("
  SELECT 
    serialNumber
    , ROUND(MAX(totalworkingHours) - MIN(totalworkingHours), 2) as total
  FROM temp.public.allData
  GROUP BY 1
  ORDER BY serialNumber
")
fromSnowflake = fromSnowflake %>% arrange(SERIALNUMBER)
df$TOTALFROMSW = as.numeric(fromSnowflake$TOTAL)
df$error = round((as.numeric(df$TOTALFROMSW) - as.numeric(df$TOTAL))/as.numeric(df$TOTALFROMSW)*100, 2)
df


## ====================================================
# Question 2) How many fields?

zonesHistograms = list()

# loop over the 3 different zones
for ( l in 1:length(limits) ) {
  lim = limits[[l]]
  
  dataFiltered = dataOrderedSampled1m %>%
    filter( GPSLONGITUDE >= lim$left &
              GPSLONGITUDE <= lim$right &
              GPSLATITUDE >= lim$bottom &
              GPSLATITUDE <= lim$top &
              IsInTheField == 1
          ) %>%
    arrange(DATETIME)
  
  xn = dataFiltered$GPSLONGITUDE
  yn = dataFiltered$GPSLATITUDE
  
  # first create ~400x400m bins
  bins_y = (lim$top - lim$bottom)/(1/240)
  bins_x = (lim$right - lim$left)/(1/240)
  
  # distance heatmap
  h=hist2d(xn, yn, nbins=c(bins_x,bins_y), same.scale=FALSE, col=c("white", heat.colors(20)))
  
  zonesHistograms[[l]] = h
}

cutoff = 120
h = zonesHistograms[[1]]$counts
nrow(h)
ncol(h)
l1 = length( h[h>cutoff] )

h = zonesHistograms[[2]]$counts
nrow(h)
ncol(h)
l2 = length( h[h>cutoff] )

h = zonesHistograms[[3]]$counts
nrow(h)
ncol(h)
l3 = length( h[h>cutoff] )

# Answer
sum( c(l1, l2, l3) )

## ====================================================
# Question 3)

distinctSerials = as.vector( pull( dataOrdered %>% distinct(SERIALNUMBER) ) )

tractorActivitiesList = list()

for ( tractorCounter in 1:length(distinctSerials) ) {

  serial = distinctSerials[tractorCounter]
  dataFiltered = dataOrderedSampled1m %>%
    filter( IsInTheField == 1 &
            SERIALNUMBER == serial
    ) %>%
    arrange(DATETIME)

  speedAbove10 = nrow(dataFiltered %>% filter(SPEED_MA_30M > 10))
  speedBelow10 = nrow(dataFiltered %>% filter(SPEED_MA_30M < 10))
  
  ptoRear  = nrow(dataFiltered %>% filter(PTOREAR_RPM > 0))
  ptoFront = nrow(dataFiltered %>% filter(PTOFRONT_RPM > 10))
    
  activityList = list(serial=serial, speedAbove10=speedAbove10, speedBelow10=speedBelow10, ptoRear=ptoRear, ptoFront=ptoFront)
  tractorActivitiesList[[tractorCounter]] = activityList
}
tractorActivitiesList

## ====================================================
# Question 6)

# fuel save
v = dataOrderedSampled1m %>% filter( dataOrderedSampled1m$IsInTheField == 0 )
f = v$FUELCONSUMPTION_L_H
f[ is.na(f) ] = 0
mean(f)*tt




# 
# singlePathN = 4
# plotSinglePath( singlePathsContainer[[singlePathN]], serial )
# nrow(singleRun)
# sum(singleRun$SYMB)-1
# max(singleRun$DATETIME) 
# min(singleRun$DATETIME)
# 
# (max(singleRun$TOTALWORKINGHOURS) - min(singleRun$TOTALWORKINGHOURS)) * 60
# 
# ( as.integer(as.POSIXct( max(singleRun$DATETIME) )) -
#   as.integer(as.POSIXct( min(singleRun$DATETIME) )) )/60
# 
# singleRun %>% distinct(SYMB)
# singleRun %>% distinct(TIMEDIFFERENCE)
# 
# View(singleRun)
# summary(singleRun)
# 
# nrow(dataOrderedSampled1m %>% filter(PTOREAR_RPM > 0) )
# 
# plotSinglePath(singleRun, serial)
# head(dataOrderedSampled1m)
# 
# dataForHCluster = dataOrderedSampled1m[, c("GPSLONGITUDE", "GPSLATITUDE", "ENGINE_RPM", "ENGINELOAD",
#                                          "FUELCONSUMPTION_L_H", "SPEEDGEARBOX_KM_H")] %>%
#                   filter(dataOrderedSampled1m$IsInTheField == 0 & 
#                          dataOrderedSampled1m$PARKINGBREAKSTATUS == 0 
#                            & dataOrderedSampled1m$TEMPAMBIENT_C > 20
#                         ) %>%
#                   drop_na()
# 
# glimpse(dataForHCluster)
# 
# 
# xf = dataForHCluster$GPSLONGITUDE
# yf = dataForHCluster$GPSLATITUDE
# zf = dataForHCluster$FUELCONSUMPTION_L_H
# d = as.data.frame( cbind(xf, yf) )
# gmap = global_map
# bins = 200
# alpha = 0.6
# title = "Fuel consumption on the road"
# 
# fin_map = ggmap(gmap) + 
#     stat_summary_2d(data = d, aes(x=xf, y=yf, z=zf ), fun = mean, alpha = alpha, bins = bins) + 
#     scale_fill_gradient(name = "Fuel consumption", low = "blue", high = "red") + 
#     ggtitle(title) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     theme(plot.title = element_text(hjust = 0.5),
#           axis.text.x = element_text(face="bold", color="#993333", size=10),
#           axis.text.y = element_text(face="bold", color="#993333", size=10),
#           panel.grid.major = element_line(colour="black", size=0.5),
#           panel.grid.minor = element_line(colour="black", size=0.5)
#     )
# 
# fin_map

