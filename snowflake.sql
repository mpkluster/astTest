DROP TABLE util_db.public.test;

USE DATABASE temp;
CREATE TABLE temp.public.test (
  DateTime STRING
  ,SerialNumber STRING
  ,GpsLongitude STRING
  ,GpsLatitude STRING
  ,TotalWorkingHours STRING
  ,Engine_rpm STRING
  ,EngineLoad STRING
  ,FuelConsumption_l_h STRING
  ,SpeedGearbox_km_h STRING
  ,SpeedRadar_km_h STRING
  ,TempCoolant_C STRING
  ,PtoFront_rpm STRING
  ,PtoRear_rpm STRING
  ,GearShift STRING
  ,TempAmbient_C STRING
  ,ParkingBreakStatus STRING
  ,DifferentialLockStatus STRING
  ,AllWheelDriveStatus STRING
  ,CreeperStatus STRING
);

CREATE TABLE temp.public.dataf AS (
    SELECT * FROM temp.public.test
    UNION ALL
    SELECT * FROM temp.public.test2
    UNION ALL
    SELECT * FROM temp.public.test3
    UNION ALL
    SELECT * FROM temp.public.test4
    UNION ALL
    SELECT * FROM temp.public.test5
    UNION ALL
    SELECT * FROM temp.public.test6
    UNION ALL
    SELECT * FROM temp.public.test7
    UNION ALL
    SELECT * FROM temp.public.test8
    UNION ALL
    SELECT * FROM temp.public.test9
);

SELECT COUNT(*) FROM temp.public.dataf; // 3,538,795 // 3,538,795

SELECT * FROM temp.public.dataf LIMIT 10;

// remove the first row
DELETE FROM temp.public.dataf WHERE datetime = 'DateTime';

DESC TABLE temp.public.dataf;

// clean the dateTime field
CREATE OR REPLACE TABLE temp.public.allData AS (
    SELECT
        (SUBSTRING(datetime,1,19))::timestamp datetime
        , serialNumber
        , gpsLongitude::double gpsLongitude
        , gpsLatitude::double gpsLatitude
        , totalWorkingHours::double totalWorkingHours
        , Engine_rpm::double Engine_rpm
        , EngineLoad::double EngineLoad
        , FuelConsumption_l_h::double FuelConsumption_l_h
        , SpeedGearbox_km_h::double SpeedGearbox_km_h
        , SpeedRadar_km_h::double SpeedRadar_km_h
        , TempCoolant_C::double TempCoolant_C
        , PtoFront_rpm::double PtoFront_rpm
        , PtoRear_rpm::double PtoRear_rpm
        , GearShift::int GearShift
        , TempAmbient_C::double TempAmbient_C
        , ParkingBreakStatus::int ParkingBreakStatus
        , DifferentialLockStatus::int DifferentialLockStatus
        , CASE WHEN AllWheelDriveStatus = 'on' THEN 'ON'
               WHEN AllWheelDriveStatus = 'off' THEN 'OFF'
               ELSE AllWheelDriveStatus
          END AS AllWheelDriveStatus
        , CreeperStatus
    FROM temp.public.dataf
);

CREATE TABLE IF NOT EXISTS temp.public.allDataBackup AS (
    SELECT * FROM temp.public.allData
);

SELECT COUNT(*) FROM temp.public.allData; // 3,538,794
SELECT COUNT(*) FROM temp.public.allDataBackup; // 3,538,794

SELECT * FROM temp.public.allData ORDER BY datetime DESC LIMIT 10;

DESC TABLE temp.public.allData;

SELECT DISTINCT serialNumber FROM temp.public.allData;
//A2302888 A7702043 A6002058 A7702039 A7702047 A6002059 A2302900 A2302959 A2302895 A7702023

SELECT 
    serialNumber
    , COUNT(DISTINCT to_date(dateTime)) daysOnRun
    , MIN(dateTime) startDate
    , MAX(dateTime) maxDate
    , MAX(totalWorkingHours) - MIN(totalWorkingHours) workingHours
FROM temp.public.allData
GROUP BY 1
ORDER BY 5 DESC;

// check if some work is done at night
SELECT
    serialNumber
    , to_date(dateTime) date
    , MIN(dateTime) AS startDay
    , MAX(dateTime) AS endDay
    , ROUND(ABS(TIMEDIFF(second, endDay, startDay))/3600,2) dayHours
    , MAX()
FROM temp.public.allData
GROUP BY 1,2
ORDER BY serialNumber, date;

// look into some samples
SELECT * FROM temp.public.allData
WHERE serialNumber = 'A2302888' AND to_date(dateTime) BETWEEN '2019-03-13' AND '2019-03-16'
//  AND DIFFERENTIALLOCKSTATUS = 0
//  AND ALLWHEELDRIVESTATUS <> 'Automatic'
ORDER BY dateTime;

SELECT GEARSHIFT, DIFFERENTIALLOCKSTATUS, ALLWHEELDRIVESTATUS, COUNT(*) cnt 
FROM temp.public.allData 
GROUP BY 1,2,3 
ORDER BY cnt DESC;

// ALLWHEELDRIVESTATUS analysis (which tractor is using what)
SELECT 
    serialNumber
    , LISTAGG(DISTINCT ALLWHEELDRIVESTATUS, ', ') FourwheelDriveStatus
FROM temp.public.allData 
WHERE ALLWHEELDRIVESTATUS IS NOT NULL
GROUP BY 1;

SELECT 
    serialNumber
    , ALLWHEELDRIVESTATUS
    , COUNT(*) cnt
FROM temp.public.allData 
WHERE ALLWHEELDRIVESTATUS IS NOT NULL
GROUP BY 1,2
ORDER BY 1, cnt DESC;


// take the csv text output of this query and copy it into https://www.gpsvisualizer.com/map_input?form=google to plot the coordinates
SELECT
    name,
    latitude,
    longitude
FROM (
    SELECT 
        SUBSTRING(dateTime::string, 1, 16) dateTimeMinutes
        , dateTime::string || '<br />' ||
        '  -  Lat: ' || gpsLatitude::string ||  '<br />' ||
        '  -  Log: ' || gpsLongitude::string || '<br/ >' ||
        '  -  WH: ' || totalWorkingHours::string || '<br />' ||
        '  -  RPM: ' || ENGINE_RPM::string || '<br />' || 
        '  -  LOAD: ' || ENGINELOAD::string || '<br />' ||
        '  -  Fuel: ' || FUELCONSUMPTION_L_H::string || '<br />' || 
        '  -  Gearbox: ' || SPEEDGEARBOX_KM_H::string || '<br />' || 
        '  -  SpeedRadar: ' || SPEEDRADAR_KM_H::string || '<br />' ||
        '  -  Parking: ' || PARKINGBREAKSTATUS || '<br />' ||
        '  -  Differentials: ' || DIFFERENTIALLOCKSTATUS || '<br />' ||
        '  -  4wheels: ' || ALLWHEELDRIVESTATUS  AS name
        , gpsLatitude as latitude
        , gpsLongitude as longitude 
        , ROW_NUMBER() OVER (PARTITION BY dateTimeMinutes ORDER BY dateTime) row_n
    FROM temp.public.allData
    WHERE serialNumber = 'A2302888' AND to_date(dateTime) BETWEEN '2019-03-13' AND '2019-03-13'
    ORDER BY dateTime
) WHERE row_n = 1 // sampling on minute level
;

// look into some other samples
SELECT
    name,
    latitude,
    longitude
FROM (
    SELECT 
        SUBSTRING(dateTime::string, 1, 16) dateTimeMinutes
        , dateTime::string || '<br />' ||
        '  -  Lat: ' || gpsLatitude::string || '<br />' || 
        '  -  Log: ' || gpsLongitude::string || '<br />' ||
        '  -  RPM: ' || ENGINE_RPM::string || '<br />' || 
        '  -  LOAD: ' || ENGINELOAD::string ||  '<br />' ||
        '  -  Fuel: ' || FUELCONSUMPTION_L_H::string ||  '<br />' ||
        '  -  Gearbox: ' || SPEEDGEARBOX_KM_H::string ||  '<br />' ||
        '  -  SpeedRadar: ' || SPEEDRADAR_KM_H::string || '<br />' ||
        '  -  Parking: ' || PARKINGBREAKSTATUS || '<br />' ||
        '  -  Differentials: ' || DIFFERENTIALLOCKSTATUS || '<br />' ||
        '  -  4wheels: ' || ALLWHEELDRIVESTATUS  AS name
        , gpsLatitude as latitude
        , gpsLongitude as longitude 
        , ROW_NUMBER() OVER (PARTITION BY dateTimeMinutes ORDER BY dateTime) row_n
    FROM temp.public.allData
    WHERE serialNumber = 'A2302900' AND to_date(dateTime) BETWEEN '2020-02-25' AND '2020-02-25'
    ORDER BY dateTime
) WHERE row_n = 1;

// get the extent distances in xy axes (in nautical miles)
SELECT 
    (MAX(gpsLatitude)-MIN(gpsLatitude))/0.0167 AS y_extent
    , (MAX(gpsLongitude)-MIN(gpsLongitude))/0.0167 AS x_extent
FROM temp.public.allData;


// get some summary
SELECT 
    serialNumber
    , median(engineLoad)
    , avg(engineLoad)
FROM temp.public.allData
GROUP BY 1;


// sampling the data every 10 minute (and remove radarSpeed and CreeperStatus)
CREATE OR REPLACE TABLE temp.public.allData_10minSampling AS (
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
        , fuel_ma_5m
        , engineLoad_ma_5m
        , speed_ma_5m
        , fuel_ma_10m
        , engineLoad_ma_10m
        , speed_ma_10m
        , fuel_ma_20m
        , engineLoad_ma_20m
        , speed_ma_20m
        , fuel_ma_30m
        , engineLoad_ma_30m
        , speed_ma_30m
    FROM (
        SELECT
            SUBSTRING(dateTime::string, 1, 15) dateTime10Minutes
            , datetime
            , serialNumber
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 30 preceding and current row) AS fuel_ma_5m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 30 preceding and current row) AS engineLoad_ma_5m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 30 preceding and current row) AS speed_ma_5m
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 60 preceding and current row) AS fuel_ma_10m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 60 preceding and current row) AS engineLoad_ma_10m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 60 preceding and current row) AS speed_ma_10m
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 120 preceding and current row) AS fuel_ma_20m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 120 preceding and current row) AS engineLoad_ma_20m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 120 preceding and current row) AS speed_ma_20m
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 180 preceding and current row) AS fuel_ma_30m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 180 preceding and current row) AS engineLoad_ma_30m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 180 preceding and current row) AS speed_ma_30m
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
            , ROW_NUMBER() OVER (PARTITION BY serialNumber, dateTime10Minutes ORDER BY dateTime) row_n
        FROM temp.public.allData
        ORDER BY dateTime
    )
    WHERE row_n = 1
    ORDER BY dateTime
);


// sampling the data every 1 minute (and remove radarSpeed and CreeperStatus)
CREATE OR REPLACE TABLE temp.public.allData_1minSampling AS (
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
        , fuel_ma_5m
        , engineLoad_ma_5m
        , speed_ma_5m
        , fuel_ma_10m
        , engineLoad_ma_10m
        , speed_ma_10m
        , fuel_ma_20m
        , engineLoad_ma_20m
        , speed_ma_20m
        , fuel_ma_30m
        , engineLoad_ma_30m
        , speed_ma_30m
    FROM (
        SELECT
            SUBSTRING(dateTime::string, 1, 16) dateTime1Minutes
            , datetime
            , serialNumber
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 30 preceding and current row) AS fuel_ma_5m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 30 preceding and current row) AS engineLoad_ma_5m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 30 preceding and current row) AS speed_ma_5m
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 60 preceding and current row) AS fuel_ma_10m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 60 preceding and current row) AS engineLoad_ma_10m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 60 preceding and current row) AS speed_ma_10m
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 120 preceding and current row) AS fuel_ma_20m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 120 preceding and current row) AS engineLoad_ma_20m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 120 preceding and current row) AS speed_ma_20m
            , avg(FUELCONSUMPTION_L_H) over (PARTITION BY serialNumber order by dateTime rows between 180 preceding and current row) AS fuel_ma_30m
            , avg(ENGINELOAD) over (PARTITION BY serialNumber order by dateTime rows between 180 preceding and current row) AS engineLoad_ma_30m
            , avg(SPEEDGEARBOX_KM_H) over (PARTITION BY serialNumber order by dateTime rows between 180 preceding and current row) AS speed_ma_30m
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
            , GearShift::int gearshift
            , TempAmbient_C
            , ParkingBreakStatus::int ParkingBreakStatus
            , DifferentialLockStatus::int DifferentialLockStatus
            , AllWheelDriveStatus
            , ROW_NUMBER() OVER (PARTITION BY serialNumber, dateTime1Minutes ORDER BY dateTime) row_n
        FROM temp.public.allData
        ORDER BY dateTime
    )
    WHERE row_n = 1
    ORDER BY dateTime
);



// tale je ta weird k je bil celo noc na njivi pr miru ... ? hidden task? skril/pokopal kaksnega mrlica nekje?
SELECT 
//    dateTime
//    dateTime::string ||
//    '  -  Lat: ' || gpsLatitude::string || 
//    '  -  Log: ' || gpsLongitude::string ||
//    '  -  RPM: ' || ENGINE_RPM::string || 
//    '  -  LOAD: ' || ENGINELOAD::string || 
//    '  -  Fuel: ' || FUELCONSUMPTION_L_H::string || 
//    '  -  Gearbox: ' || SPEEDGEARBOX_KM_H::string || 
//    '  -  SpeedRadar: ' || SPEEDRADAR_KM_H::string ||
//    '  -  Parking: ' || PARKINGBREAKSTATUS ||
//    '  -  Differentials: ' || DIFFERENTIALLOCKSTATUS ||
//    '  -  4wheels: ' || ALLWHEELDRIVESTATUS  AS name
//    , gpsLatitude as latitude
//    , gpsLongitude as longitude
//    , totalWorkingHours
//    , Engine_rpm
//    , EngineLoad
//    , FuelConsumption_l_h
//    , SpeedGearbox_km_h
//    , SpeedRadar_km_h
//    , TempCoolant_C
//    , PtoFront_rpm
//    , PtoRear_rpm
//    , GearShift
//    , TempAmbient_C
//    , ParkingBreakStatus
//    , DifferentialLockStatus
//    , AllWheelDriveStatus
//    , CreeperStatus
    *
FROM temp.public.allData_1minSampling
WHERE dateTime >= '2019-03-04 21:41:00' 
  AND dateTime <= '2019-03-05 09:13:04' 
ORDER BY dateTime;

// serials
//A2302888 A7702043 A6002058 A7702039 A7702047 A6002059 A2302900 A2302959 A2302895 A7702023

// looking into some stuff for insights
SELECT * FROM temp.public.A2302895_1min 
WHERE ParkingBreakStatus = 0 AND 
ORDER BY dateTime;
SELECT DISTINCT serialNumber FROM temp.public.allData;
SELECT (MAX(speedRadar_km_h)::int/10)::int*10 AS speed FROM temp.public.allData;
SELECT serialNumber, COUNT(*) eventsRecorded FROM temp.public.allData GROUP BY 1 ORDER BY 2 DESC;
SELECT 14199*14199;
SELECT 
    ((speedRadar_km_h)::int/10)::int*10 AS speed 
    , ((SPEEDGEARBOX_KM_H)::int/10)::int*10 AS speedGear
    , COUNT(*) cnt
FROM temp.public.allData
GROUP BY 1,2
ORDER BY speed, speedGear, cnt;


// there seems to be something wrong with the speed radar (showing > 131.07 km/h)
SELECT 
    dateTime
    , serialNumber
    , gpsLongitude
    , gpsLatitude
    , totalWorkingHours
    , Engine_rpm
    , EngineLoad
    , FuelConsumption_l_h
    , SpeedGearbox_km_h
    , SpeedRadar_km_h
    , TempCoolant_C
    , PtoFront_rpm
    , PtoRear_rpm
    , GearShift
    , TempAmbient_C
    , ParkingBreakStatus
    , DifferentialLockStatus
    , AllWheelDriveStatus
    , CreeperStatus
    , ((speedRadar_km_h)::int/10)::int*10 AS speed
FROM (
    SELECT
        // SUBSTRING(dateTime::string, 1, 13) dateTimeHour
        SUBSTRING(dateTime::string, 1, 15) dateTime10Minutes
        , dateTime
        , serialNumber
        , gpsLongitude
        , gpsLatitude
        , totalWorkingHours
        , Engine_rpm
        , EngineLoad
        , FuelConsumption_l_h
        , SpeedGearbox_km_h
        , SpeedRadar_km_h
        , TempCoolant_C
        , PtoFront_rpm
        , PtoRear_rpm
        , GearShift
        , TempAmbient_C
        , ParkingBreakStatus
        , DifferentialLockStatus
        , AllWheelDriveStatus
        , CreeperStatus
        , ROW_NUMBER() OVER (PARTITION BY serialNumber, dateTime10Minutes ORDER BY dateTime) row_n
    FROM temp.public.allData
    WHERE serialNumber = 'A7702023'
    ORDER BY dateTime
) WHERE row_n = 1 AND speed > 100
ORDER BY dateTime, serialNumber;
// 130km/h sketchy: A7702043 (~3000), A6002058 (~5000), A7702039 (~3000), A7702047 (~2500), A6002059 (~3000), A7702023 (~2500)


// check if the effect of the speed radar error is affecting the data
SELECT AVG(ABS(speedRadar_km_h-SPEEDGEARBOX_KM_H)) FROM temp.public.allData;

SELECT 
    COUNT(*) cnt
    , MIN(speedRadar_km_h)
    , MAX(speedRadar_km_h)
    , SUM(ParkingBreakStatus) parked
    , MAX(FuelConsumption_l_h) maxFuel 
    , SUM( IFF(CreeperStatus = 'OFF', 1, 0) ) creepOff
    , cnt-creepOff AS creepOn
FROM temp.public.allData 
WHERE speedRadar_km_h > 100 
ORDER BY dateTime;


// get a complete summary of the attributes
WITH tmp_GearShift AS (
    WITH tmp1 AS (
        SELECT
            serialNumber
            , GearShift
            , COUNT(*) cnt
        FROM temp.public.allData
        GROUP BY 1,2
        ORDER BY 1, cnt DESC
    )
    SELECT
        serialNumber
        , ARRAY_AGG( OBJECT_CONSTRUCT('gear', GearShift, 'cnt', cnt) ) WITHIN GROUP (ORDER BY cnt DESC) gears 
    FROM tmp1
    GROUP BY serialNumber
)
, tmp_DifferentialLockStatus AS ( 
    WITH tmp1 AS (
        SELECT
            serialNumber
            , DifferentialLockStatus
            , COUNT(*) cnt
        FROM temp.public.allData
        GROUP BY 1,2
        ORDER BY 1, cnt DESC
    )
    SELECT
        serialNumber
        , ARRAY_AGG( OBJECT_CONSTRUCT('differential', DifferentialLockStatus, 'cnt', cnt) ) WITHIN GROUP (ORDER BY cnt DESC) differentials 
    FROM tmp1
    GROUP BY serialNumber
)
, tmp_AllWheelDriveStatus AS ( 
    WITH tmp1 AS (
        SELECT
            serialNumber
            , AllWheelDriveStatus
            , COUNT(*) cnt
        FROM temp.public.allData
        GROUP BY 1,2
        ORDER BY 1, cnt DESC
    )
    SELECT
        serialNumber
        , ARRAY_AGG( OBJECT_CONSTRUCT('4wheelDrive', AllWheelDriveStatus, 'cnt', cnt) ) WITHIN GROUP (ORDER BY cnt DESC) wheel4drive 
    FROM tmp1
    GROUP BY serialNumber
)
, tmp AS (
    SELECT
        serialNumber
        , COUNT(*) cnt
        , MIN(SPEEDGEARBOX_KM_H) minGearboxSpeed
        , MAX(SPEEDGEARBOX_KM_H) maxSPEEDGEARBOX
        , MIN(speedRadar_km_h) minRadar
        , MAX(speedRadar_km_h) maxRadar
        , SUM(ParkingBreakStatus) parked
        , MAX(FuelConsumption_l_h) maxFuel 
        , SUM( IFF(CreeperStatus = 'OFF', 1, 0) ) creepOff
        , cnt-creepOff AS creepOn
        , MIN(TempCoolant_C) minTempC
        , MAX(TempCoolant_C) maxTempC
        , MEDIAN(TempCoolant_C) avgTempC
        , MIN(PtoFront_rpm) minFront
        , MAX(PtoFront_rpm) maxFront
        , MEDIAN(PtoFront_rpm) avgFront
        , SUM(IFF(PtoFront_rpm > 0, 1, 0)) frontPositive
        , SUM(IFF(PtoFront_rpm = 0, 1, 0)) frontNulls
        , MIN(PtoRear_rpm) minRear
        , MAX(PtoRear_rpm) maxRear
        , MEDIAN(PtoRear_rpm) avgRear
        , SUM(IFF(PtoRear_rpm > 0, 1, 0)) rearPositive
        , SUM(IFF(PtoRear_rpm = 0, 1, 0)) rearNulls
        , MIN(TempAmbient_C) minTemp
        , MAX(TempAmbient_C) maxTemp
        , MEDIAN(TempAmbient_C) avgTemp
    FROM temp.public.allData 
    WHERE EngineLoad > 60 
    GROUP BY serialNumber
    ORDER BY maxFuel
)
SELECT
    tmp.serialNumber
    , cnt
    , minGearboxSpeed
    , maxSPEEDGEARBOX
    , maxRadar
    , minRadar
    , parked
    , maxFuel 
    , creepOff
    , creepOn
    , minTempC
    , maxTempC
    , avgTempC
    , minFront
    , maxFront
    , avgFront
    , frontPositive
    , frontNulls
    , minRear
    , maxRear
    , avgRear
    , rearPositive
    , rearNulls
    , minTemp
    , maxTemp
    , avgTemp
    , gears
    , differentials
    , wheel4drive
FROM tmp
LEFT JOIN tmp_GearShift ON tmp_GearShift.serialNumber = tmp.serialNumber
LEFT JOIN tmp_DifferentialLockStatus ON tmp_DifferentialLockStatus.serialNumber = tmp.serialNumber
LEFT JOIN tmp_AllWheelDriveStatus ON tmp_AllWheelDriveStatus.serialNumber = tmp.serialNumber;

// analyze creeper status for this tractor A2302888 (get a sample)
SELECT 
    name
    , gpsLongitude
    , gpsLatitude
FROM (
    SELECT
        SUBSTRING(dateTime::string, 1, 16) dateTime1Minutes
        , datetime
        , dateTime::string || '<br />' ||
        '  -  Lat: ' || gpsLatitude::string ||  '<br />' ||
        '  -  Log: ' || gpsLongitude::string || '<br/ >' ||
        '  -  Ambient temp: ' || COALESCE(TempAmbient_C::string, 'nil') || '<br />' ||
        '  -  WH: ' || totalWorkingHours::string || '<br />' ||
        '  -  RPM: ' || ENGINE_RPM::string || '<br />' || 
        '  -  LOAD: ' || ENGINELOAD::string || '<br />' ||
        '  -  Fuel: ' || FUELCONSUMPTION_L_H::string || '<br />' || 
        '  -  Gearbox: ' || SPEEDGEARBOX_KM_H::string || '<br />' || 
        '  -  SpeedRadar: ' || SPEEDRADAR_KM_H::string || '<br />' ||
        '  -  GearShift: ' || COALESCE(GearShift::string, 'nil') || '<br />' ||
        '  -  PTO Front: ' || COALESCE(PtoFront_rpm::string, 'nil') || '<br />' || 
        '  -  PTO Rear: ' || COALESCE(PtoRear_rpm::string, 'nil') || '<br />' ||
        '  -  TempCoolant: ' || COALESCE(TempCoolant_C::string, 'nil') || '<br />' ||
        '  -  Parking: ' || COALESCE(PARKINGBREAKSTATUS, 'nil') || '<br />' ||
        '  -  Differentials: ' || COALESCE(DIFFERENTIALLOCKSTATUS, 'nil') || '<br />' ||
        '  -  4wheels: ' || COALESCE(ALLWHEELDRIVESTATUS, 'nil')  AS name
        , gpsLongitude
        , gpsLatitude
        , ROW_NUMBER() OVER (PARTITION BY serialNumber, dateTime1Minutes ORDER BY dateTime) row_n
    FROM temp.public.allData
    WHERE serialNumber = 'A2302888' AND CreeperStatus IS NULL
      AND to_date(dateTime) = '2020-04-18'
    ORDER BY dateTime
) WHERE row_n = 1 
ORDER BY dateTime;


// compute the moving average to get an idea where the tractor is working or is driving on the road
SELECT 
    name
    , gpsLongitude
    , gpsLatitude
FROM (
    SELECT
        SUBSTRING(dateTime::string, 1, 16) dateTime1Minutes
        , datetime
        , avg(FUELCONSUMPTION_L_H) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS fuel_ma
        , avg(ENGINELOAD) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS engineLoad_ma
        , avg(SPEEDGEARBOX_KM_H) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS speed_ma
        , dateTime::string || '<br />' ||
        '  -  Lat: ' || gpsLatitude::string ||  '<br />' ||
        '  -  Log: ' || gpsLongitude::string || '<br/ >' ||
        '  -  Ambient temp: ' || COALESCE(TempAmbient_C::string, 'nil') || '<br />' ||
        '  -  WH: ' || totalWorkingHours::string || '<br />' ||
        '  -  RPM: ' || ENGINE_RPM::string || '<br />' || 
        '  -  LOAD: ' || ENGINELOAD::string || '<br />' ||
        '  -  LOAD MA: ' || engineLoad_ma::string || '<br />' ||
        '  -  Fuel: ' || FUELCONSUMPTION_L_H::string || '<br />' || 
        '  -  Fuel MA: ' || fuel_ma::string || '<br />' || 
        '  -  Gearbox: ' || SPEEDGEARBOX_KM_H::string || '<br />' || 
        '  -  SpeedRadar: ' || SPEEDRADAR_KM_H::string || '<br />' ||
        '  -  GearShift: ' || COALESCE(GearShift::string, 'nil') || '<br />' ||
        '  -  PTO Front: ' || COALESCE(PtoFront_rpm::string, 'nil') || '<br />' || 
        '  -  PTO Rear: ' || COALESCE(PtoRear_rpm::string, 'nil') || '<br />' ||
        '  -  TempCoolant: ' || COALESCE(TempCoolant_C::string, 'nil') || '<br />' ||
        '  -  Parking: ' || COALESCE(PARKINGBREAKSTATUS, 'nil') || '<br />' ||
        '  -  Differentials: ' || COALESCE(DIFFERENTIALLOCKSTATUS, 'nil') || '<br />' ||
        '  -  4wheels: ' || COALESCE(ALLWHEELDRIVESTATUS, 'nil')  AS name
        , gpsLongitude
        , gpsLatitude
        , ROW_NUMBER() OVER (PARTITION BY serialNumber, dateTime1Minutes ORDER BY dateTime) row_n
    FROM temp.public.allData 
    WHERE serialNumber = 'A6002059' //'A2302888'
      //AND to_date(dateTime) = '2020-04-18'
    ORDER BY dateTime
) WHERE row_n = 1 //AND fuel_ma > 30 AND engineload_ma > 60 AND speed_ma < 20
ORDER BY dateTime
LIMIT 1000;

// try to find if there is some seasonality visible in the data
WITH tmp AS (
    SELECT
        serialNumber
        , SUBSTRING(dateTime::string, 1, 7) AS month
        , AVG(FUELCONSUMPTION_L_H) avgFuelConsumption
    FROM temp.public.allData
    WHERE month > '2018-11'
    GROUP BY 1,2
)
SELECT
    *
FROM tmp
PIVOT(sum(avgFuelConsumption) for month in ('2018-12','2019-01','2019-02','2019-03','2019-04','2019-05','2019-06','2019-07','2019-08','2019-09','2019-10','2019-11','2019-12','2020-01','2020-02','2020-03','2020-04'))
      as p (serialNumber, dec2018, jan2019, feb2019, mar2019, apr2019, may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019, jan2020, feb2020, mar2020, apr2020)
;

// pivot over serial number
WITH tmp AS (
    SELECT
        serialNumber
        , SUBSTRING(dateTime::string, 1, 7) AS month
        , AVG(FUELCONSUMPTION_L_H) avgFuelConsumption
    FROM temp.public.allData
    GROUP BY 1,2
)
SELECT
    *
FROM tmp
PIVOT(sum(avgFuelConsumption) for serialNumber in ('A2302888', 'A2302895', 'A2302900', 'A2302959', 'A6002058', 'A6002059', 'A7702023', 'A7702039', 'A7702043', 'A7702047'))
      as p (month, A2302888, A2302895, A2302900, A2302959, A6002058, A6002059, A7702023, A7702039, A7702043, A7702047)
ORDER BY month
;
// help with this
// SELECT '\'' || (LISTAGG(DISTINCT serialNUmber, '\', \'') WITHIN GROUP (ORDER BY serialNUmber))::string || '\'' FROM temp.public.allData;

// => no seasonality detected, but a drop in working hours occurred on june 2019


// do some more breakdown by month to see if there is some difference in the working pattern of the tractors by month
WITH tmp AS (
    SELECT
        serialNumber
        , to_date(dateTime) day
        , MIN(dateTime) minTIme
        , MAX(dateTime) maxTime
        , SUM(IFF(speed_ma_30m>10 AND fuel_ma_30m > 30, 1, 0)) above10 //ABS(timediff(seconds, maxtime, mintime)/3600) AS dayWorkingHours
        , SUM(IFF(speed_ma_30m<10 AND fuel_ma_30m > 30, 1, 0)) below10
        , ROUND(above10/below10, 5) ratio
        , COUNT(*) events
    FROM temp.public.allData_1minSampling //temp.public.allData
    GROUP BY 1,2
    ORDER BY 1,2
)
, tmp2 AS (
    SELECT
        serialNumber
        , SUBSTRING(day::string, 1, 7) AS month
        , ROUND(SUM(above10)/NULLIF(SUM(below10),0), 5) metric
//        , SUM(dayWorkingHours) AS metric
//        , SUM(events) AS metric
    FROM tmp
    GROUP BY 1,2
)
SELECT
    *
FROM tmp2
PIVOT(sum(metric) for month in ('2018-12','2019-01','2019-02','2019-03','2019-04','2019-05','2019-06','2019-07','2019-08','2019-09','2019-10','2019-11','2019-12','2020-01','2020-02','2020-03','2020-04'))
      as p (serialNumber, dec2018, jan2019, feb2019, mar2019, apr2019, may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019, jan2020, feb2020, mar2020, apr2020)
;

// explore the fuel consumption
SELECT 
    serialNumber
    , MAX(FUELCONSUMPTION_L_H) maxFuel
    , AVG(FUELCONSUMPTION_L_H) avg
    , MEDIAN(FUELCONSUMPTION_L_H) median
    , COUNT(*) eventcnt
FROM temp.public.allData
GROUP BY 1
//HAVING maxFuel>60
ORDER BY maxFuel;


// find days where all tractors were active at the same time
SELECT
    SUBSTRING(dateTime::string, 1, 13) dateTimeHour
    , COUNT(DISTINCT serialNumber) numOfTractorsActive
FROM temp.public.allData 
GROUP BY 1
HAVING numOfTractorsActive > 1
ORDER BY 1;

// plot this on geoLocator
SELECT
    dateTime::string || '  -  ' || serialNumber::string as name
    , gpsLongitude
    , gpsLatitude
FROM temp.public.allData
WHERE SUBSTRING(dateTime::string, 1, 13) = '2020-03-19 09'
ORDER BY serialNumber, dateTime;

// distinct gearshift
SELECT DISTINCT GearShift FROM temp.public.allData;

// some other summary
SELECT
    serialNumber
    , MIN(to_date(dateTime)) startDate
    , MAX(to_date(dateTime)) enDate
    , MAX(speedRadar_km_h) maxRadarSpeed
    , MAX(SPEEDGEARBOX_KM_H) maxSPEEDGEARBOX
    , AVG(SPEEDGEARBOX_KM_H) avgSPEEDGEARBOX
    , MEDIAN(SPEEDGEARBOX_KM_H) medSPEEDGEARBOX
    , MAX(ENGINE_RPM) maxEngineRPM
    , AVG(engine_rpm) avgEngineRPM
    , MEDIAN(engine_rpm) medEngineRPM
    , AVG(engineLoad) avgEngineLoad
    , MEDIAN(engineLoad) medEngineLoad
    , MAX(FUELCONSUMPTION_L_H) maxFUELCONSUMPTION
    , AVG(FUELCONSUMPTION_L_H) avgFUELCONSUMPTION
    , MEDIAN(FUELCONSUMPTION_L_H) medFUELCONSUMPTION
    , MIN(TOTALWORKINGHOURS) minTOTALWORKINGHOURS
    , max(TOTALWORKINGHOURS) maxTOTALWORKINGHOURS
FROM temp.public.allData
GROUP BY 1
ORDER BY maxFUELCONSUMPTION
;


// Q: what is happening on the left edge of the map? A: nothing ... a tractor decided to take a long journey
SELECT
    name,
    latitude,
    longitude
FROM (
    SELECT
        avg(FUELCONSUMPTION_L_H) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS fuel_ma
        , avg(ENGINELOAD) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS engineLoad_ma
        , avg(SPEEDGEARBOX_KM_H) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS speed_ma
        , dateTime::string || '<br />' ||
        '  -  Lat: ' || gpsLatitude::string ||  '<br />' ||
        '  -  Log: ' || gpsLongitude::string || '<br/ >' ||
        '  -  Ambient temp: ' || COALESCE(TempAmbient_C::string, 'nil') || '<br />' ||
        '  -  WH: ' || totalWorkingHours::string || '<br />' ||
        '  -  RPM: ' || ENGINE_RPM::string || '<br />' || 
        '  -  LOAD: ' || ENGINELOAD::string || '<br />' ||
        '  -  LOAD MA: ' || engineLoad_ma::string || '<br />' ||
        '  -  Fuel: ' || FUELCONSUMPTION_L_H::string || '<br />' || 
        '  -  Fuel MA: ' || fuel_ma::string || '<br />' || 
        '  -  Gearbox speed: ' || SPEEDGEARBOX_KM_H::string || '<br />' || 
        '  -  Gearbox speed MA: ' || speed_ma::string || '<br />' || 
        '  -  GearShift: ' || COALESCE(GearShift::string, 'nil') || '<br />' ||
        '  -  PTO Front: ' || COALESCE(PtoFront_rpm::string, 'nil') || '<br />' || 
        '  -  PTO Rear: ' || COALESCE(PtoRear_rpm::string, 'nil') || '<br />' ||
        '  -  TempCoolant: ' || COALESCE(TempCoolant_C::string, 'nil') || '<br />' ||
        '  -  Parking: ' || COALESCE(PARKINGBREAKSTATUS::string, 'nil') || '<br />' ||
        '  -  Differentials: ' || COALESCE(DIFFERENTIALLOCKSTATUS::string, 'nil') || '<br />' ||
        '  -  4wheels: ' || COALESCE(ALLWHEELDRIVESTATUS::string, 'nil')  AS name
        , gpsLatitude as latitude
        , gpsLongitude as longitude
    FROM temp.public.allData_1minsampling
    WHERE gpsLatitude < 19.5
    ORDER BY dateTime
);


SELECT COUNT(*) FROM temp.public.allData_10minSampling; // 64, 328

SELECT
    name,
    latitude,
    longitude
FROM (
    SELECT
        avg(FUELCONSUMPTION_L_H) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS fuel_ma
        , avg(ENGINELOAD) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS engineLoad_ma
        , avg(SPEEDGEARBOX_KM_H) over (partition by serialNumber order by dateTime rows between 60 preceding and current row) AS speed_ma
        , dateTime::string || '<br />' ||
        '  -  Lat: ' || gpsLatitude::string ||  '<br />' ||
        '  -  Log: ' || gpsLongitude::string || '<br/ >' ||
        '  -  Ambient temp: ' || COALESCE(TempAmbient_C::string, 'nil') || '<br />' ||
        '  -  WH: ' || totalWorkingHours::string || '<br />' ||
        '  -  RPM: ' || ENGINE_RPM::string || '<br />' || 
        '  -  LOAD: ' || ENGINELOAD::string || '<br />' ||
        '  -  LOAD MA: ' || engineLoad_ma::string || '<br />' ||
        '  -  Fuel: ' || FUELCONSUMPTION_L_H::string || '<br />' || 
        '  -  Fuel MA: ' || fuel_ma::string || '<br />' || 
        '  -  Gearbox speed: ' || SPEEDGEARBOX_KM_H::string || '<br />' || 
        '  -  Gearbox speed MA: ' || speed_ma::string || '<br />' || 
        '  -  GearShift: ' || COALESCE(GearShift::string, 'nil') || '<br />' ||
        '  -  PTO Front: ' || COALESCE(PtoFront_rpm::string, 'nil') || '<br />' || 
        '  -  PTO Rear: ' || COALESCE(PtoRear_rpm::string, 'nil') || '<br />' ||
        '  -  TempCoolant: ' || COALESCE(TempCoolant_C::string, 'nil') || '<br />' ||
        '  -  Parking: ' || COALESCE(PARKINGBREAKSTATUS::string, 'nil') || '<br />' ||
        '  -  Differentials: ' || COALESCE(DIFFERENTIALLOCKSTATUS::string, 'nil') || '<br />' ||
        '  -  4wheels: ' || COALESCE(ALLWHEELDRIVESTATUS::string, 'nil')  AS name
        , gpsLatitude as latitude
        , gpsLongitude as longitude
    FROM temp.public.allData_10minsampling 
    WHERE gpsLatitude > 45.55 AND gpsLatitude < 45.70 AND gpsLongitude > 20.1 AND gpsLongitude < 20.45
    ORDER BY dateTime
)// WHERE fuel_ma >= 30;
;


