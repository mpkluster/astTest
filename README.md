# Files

<p>snowflake.sql  -- data cleaning and exploratory SQL code</p>
<p>run.R          -- data analysis script</p>

# Requirements

To run the R code, you need snowflake access at https://ke72837.eu-central-1.snowflakecomputing.com and an active google cloud account for accessing the google API. Set up the credentials for both these two accounts in the credentials.R file (create it and put this file in the folder of the repo) with just these two lines:

snowflakePasswordStoredInCredentialsFile = "<snowflake password>"<br />
googleApiKeyStoredInCredentialsFile      = "<your google API key>"

