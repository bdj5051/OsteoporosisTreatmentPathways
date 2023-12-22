library(ODTP)
 

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()
 
# The folder where the study intermediate and result files will be written:
 outputFolder <- "s:/ODTP"
 
# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "s:/andromeda")
 
# Details for connecting to the server:
 connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                 server = Sys.getenv("PDW_SERVER"),
                                                                 user = NULL,
                                                                 password = NULL,
                                                                 port = Sys.getenv("PDW_PORT"))
 renv
# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "CDM_IBM_MDCD_V1153.dbo"
 
# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_skeleton"

# # Some meta-information that will be used by the export function:
databaseId <- ""
StartDate <- "" # The start date of CDM-transformation
EndDate <- "" # The end date of CDM-transformation
# # For Oracle: define a schema that can be used to emulate temp tables:
oracleTempSchema <- NULL

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = oracleTempSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseId,
        databaseDescription = databaseId,
        startDate = StartDate,
        endDate = EndDate,
        createCohorts = TRUE,
        synthesizePositiveControls = FALSE,
        runAnalyses = TRUE,
        packageResults = TRUE,
        maxCores = maxCores)


zip::zip(zipfile = paste0(databaseId, "_results.zip"), files = "results", root = outputFolder)
