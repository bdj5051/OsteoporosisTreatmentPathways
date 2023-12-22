#' Run treatment pathway Analysis
#'
#' @description
#' Run the treatment pathways analysis code
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createconnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the exposure and outcome cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder where the results were generated; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @export

runPathwayAnalysis <- function(connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable,
                               databaseName,
                               outputFolder,
                               startDate = startDate,
                               endDate = endDate
                               ){

  tpOutputFolder <- file.path(outputFolder, "results/TreatmentPathways")
  if (!file.exists(tpOutputFolder)){
    dir.create(tpOutputFolder)
  }

  connection <- DatabaseConnector::connect(connectionDetails)

  # Create treatment pathway table structure:
  sql <- SqlRender::loadRenderTranslateSql("CreatePathwaysTable.sql",
                                           "ODTP",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                           cohortTable = cohortTable)
  DatabaseConnector::executeSql(connection=connection, sql=sql)

  # Create treatment pathway code table:
  codeTable <- read.csv(file.path(getwd(), "inst/settings/PathwayAnalysisCodes.csv"))

  # Instantiate analysis:
  ParallelLogger::logInfo("Running treatment pathway analysis")
  cohortsToCreate <- read.csv(file.path("inst", "settings", "CohortsToCreate.csv"))
  cohortsToCreate <- cohortsToCreate[cohortsToCreate$cohortType=='P_Target',]
  pathway_results <- data.frame()
  for (id in cohortsToCreate$cohortId){
    ParallelLogger::logInfo(paste0("Execute pathways :", cohortsToCreate[cohortsToCreate$cohortId==id,]$atlasName))
    excSql <- SqlRender::loadRenderTranslateSql("TreatmentPathways.sql",
                                                "ODTP",
                                                dbms = connectionDetails$dbms,
                                                oracleTempSchema = oracleTempSchema,
                                                cohortDatabaseSchema = cohortDatabaseSchema,
                                                cohortTable = cohortTable,
                                                cohortId = id,
                                                generationId=id)
    DatabaseConnector::executeSql(connection = connection, sql = excSql, progressBar = TRUE)

    resultSql <- paste("select *",
                       "from @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_paths t1",
                       "where pathway_analysis_generation_id=@generationId;")
    resultSql <- SqlRender::render(sql = resultSql,
                                   generationId=id,
                                   cohortDatabaseSchema=cohortDatabaseSchema,
                                   cohortTable=cohortTable)
    result <- DatabaseConnector::querySql(connection = connection, sql = resultSql)
    pathway_results <- rbind(pathway_results, result)
  }

  pathway_results <- pathway_results %>%
    left_join(codeTable, by=c("STEP_1"="code")) %>% mutate(STEP_1=name) %>% select(PATHWAY_ANALYSIS_GENERATION_ID, TARGET_COHORT_ID, STEP_1, STEP_2, STEP_3, COUNT_VALUE) %>%
    left_join(codeTable, by=c("STEP_2"="code")) %>% mutate(STEP_2=name) %>% select(PATHWAY_ANALYSIS_GENERATION_ID, TARGET_COHORT_ID, STEP_1, STEP_2, STEP_3, COUNT_VALUE)  %>%
    left_join(codeTable, by=c("STEP_3"="code")) %>% mutate(STEP_3=name) %>% select(PATHWAY_ANALYSIS_GENERATION_ID, TARGET_COHORT_ID, STEP_1, STEP_2, STEP_3, COUNT_VALUE)

  write.csv(x = pathway_results, file = file.path(tpOutputFolder, "pathways.csv"))

  # RULE1) There is no combination treatment!
  cleaned_pathway_results <- pathway_results %>%
    mutate(
      STEP_1=ifelse(grepl("\\+", STEP_1),
                    ifelse(stringr::str_detect(STEP_1, stringr::fixed(STEP_2))&!is.na(STEP_2), stringr::str_replace(gsub("\\+", "", STEP_1), stringr::fixed(STEP_2), ""), STEP_1), STEP_1),
      STEP_2=ifelse(grepl("\\+", STEP_2),
                    ifelse(stringr::str_detect(STEP_2, stringr::fixed(STEP_1)), stringr::str_replace(gsub("\\+", "", STEP_2), stringr::fixed(STEP_1),""),
                           ifelse(stringr::str_detect(STEP_2, stringr::fixed(STEP_3)) & !is.na(STEP_3), stringr::str_replace(gsub("\\+", "", STEP_2), stringr::fixed(STEP_3), ""), STEP_2)), STEP_2),
      STEP_3=ifelse(grepl("\\+", STEP_3),
                    ifelse(stringr::str_detect(STEP_3, stringr::fixed(STEP_2))&!is.na(STEP_2), stringr::str_replace(gsub("\\+", "", STEP_3), stringr::fixed(STEP_2), ""), STEP_3), STEP_3),
      STEP_2=ifelse(STEP_1==STEP_2, NA, STEP_2),
      STEP_3=ifelse(STEP_2==STEP_3, NA, STEP_3)) %>%
    group_by(PATHWAY_ANALYSIS_GENERATION_ID, TARGET_COHORT_ID, STEP_1, STEP_2, STEP_3) %>%
    summarise(personCount=sum(COUNT_VALUE)) %>%
    as.data.frame()
  write.csv(x=cleaned_pathway_results, file = file.path(tpOutputFolder, "cleaned_pathway.csv"))


  # Create sequential treatment table:
  ParallelLogger::logInfo("Creating Sequential Treatment Table")
  sql <- SqlRender::loadRenderTranslateSql("createSequentialTreatmentTable.sql",
                                           "ODTP",
                                           dbms = connectionDetails$dbms,
                                           cdmDatabaseSchema=cdmDatabaseSchema,
                                           oracleTempSchema = oracleTempSchema,
                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                           cohortTable = cohortTable)
  DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = TRUE)

  base.sql <- "select * from @cohortDatabaseSchema.@cohortTable_PRESCRIPTION_EVENTS;"
  base.sql <- SqlRender::render(sql = base.sql,
                                cohortDatabaseSchema=cohortDatabaseSchema,
                                cohortTable=cohortTable)
  base_data <- DatabaseConnector::querySql(connection = connection, sql = base.sql)
  saveRDS(base_data, file.path(outputFolder, "tmpData/PrescriptionEvents_Whole.RDS"))

  base.sql <- "select * from @cohortDatabaseSchema.@cohortTable_PRESCRIPTION_EVENTS where line != 0;"
  base.sql <- SqlRender::render(sql = base.sql,
                                cohortDatabaseSchema=cohortDatabaseSchema,
                                cohortTable=cohortTable)
  base_data <- DatabaseConnector::querySql(connection = connection, sql = base.sql)
  saveRDS(base_data, file.path(outputFolder, "tmpData/PrescriptionEvents_line.RDS"))

  # Extract Sub Results
  extractSubResults(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable,
                    outputFolder=outputFolder,
                    startDate = startDate,
                    endDate = endDate)
}
