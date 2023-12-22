#' Extract Results
#'
#' @details
#' Extract Duration, Demographics etc. Results.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
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
extractSubResults <- function(connectionDetails,
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              outputFolder,
                              startDate = startDate,
                              endDate = endDate){

  whole_data <- readRDS(file.path(outputFolder, "tmpData/PrescriptionEvents_Whole.RDS"))
  whole_data <- whole_data %>% filter(P_RANK==1) %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           LAST_DRUG_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(LAST_DRUG_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-LAST_DRUG_DATE))
  whole_results <- whole_data %>% group_by(LINE) %>%
    summarise(p_cnt=n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"))
  write.csv(whole_results, file = file.path(outputFolder, "results/TreatmentPathways/whole_duration.csv"))
  rm(whole_data)

  '%!in%' <- Negate('%in%')
  yearEndDate <-  as.Date(endDate)
  tmpDir <- file.path(outputFolder, "tmpData")
  # treatment_duration -------------------------------------------
  line_data <- readRDS(file.path(outputFolder, "tmpData/PrescriptionEvents_line.RDS"))


  # treatment_duration (FIRST_LINE) -------------------------------------------
  # If a prescription gap exceeds one year, it is considered treatment discontinuation (exclude BPs)
  first_line_2 <- line_data %>% filter(LINE==1 & COHORT_DEFINITION_ID %!in% c(1001, 1002)) %>%
    group_by(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LAST_VISIT_DATE, LINE_START_DATE, LINE_END_DATE) %>%
    mutate(NEXT_DRUG_EXPOSURE_DATE = lag(DRUG_EXPOSURE_DATE, 1),
           NEXT_INVALID = ifelse(as.numeric(NEXT_DRUG_EXPOSURE_DATE-DRUG_EXPOSURE_DATE)<=365, 0, 1)) %>%
    filter(NEXT_INVALID == 1) %>%
    group_by(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LAST_VISIT_DATE, LINE_START_DATE, LINE_END_DATE) %>%
    filter(P_RANK==max(P_RANK)) %>% as.data.frame() %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           TREATMENT_END_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(TREATMENT_END_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-TREATMENT_END_DATE)) %>% #Discontinuity began after treatment end date (23.10.12)
    select(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, LAST_DRUG_EXPOSURE_DATE, TREATMENT_END_DATE, TREATMENT_DURATION, TREATMENT_DISCONTINUE)

  # If the line changes, it is considered discontinue (only BPs)
  first_line_3 <- line_data %>% filter(LINE==1 & P_RANK==1 & SUBJECT_ID %!in% first_line_2$SUBJECT_ID) %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           TREATMENT_END_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(TREATMENT_END_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-TREATMENT_END_DATE)) %>%
    select(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, LAST_DRUG_EXPOSURE_DATE, TREATMENT_END_DATE, TREATMENT_DURATION, TREATMENT_DISCONTINUE)

  # First Line Data set
  first_line <- rbind(first_line_2, first_line_3)
  first_line_cnt <- line_data %>% left_join(first_line) %>%
    filter(LINE==1 & DRUG_EXPOSURE_DATE <= LAST_DRUG_EXPOSURE_DATE) %>%
    group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
    summarise(PRECRIPTION_CNT =  sum(DAYS_SUPPLY)) %>%
    select(COHORT_DEFINITION_ID, SUBJECT_ID, PRECRIPTION_CNT)
  first_line <- first_line %>% left_join(first_line_cnt)
  first_line <- first_line %>% mutate(INVALID = ifelse(as.numeric(yearEndDate - LINE_START_DATE)<365,1,0)) # minimum observed time is 365 days.

  saveRDS(object = first_line, file = file.path(outputFolder, "tmpData/first_line.RDS"))

  # Duration of Treatment
  first_result1 <- first_line %>% filter(INVALID == 0) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'First_Line_ALL',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  first_result2 <- first_line %>% filter(TREATMENT_DURATION < 340 & INVALID == 0) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'First_Line_Under',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  first_result3 <- union_all(first_line[first_line$COMBO==1 & first_line$TREATMENT_DURATION < 340 & first_line$TREATMENT_DISCONTINUE > 180 & first_line$INVALID == 0 & first_line$COHORT_DEFINITION_ID %!in% c(1001, 1002),],
                             first_line[first_line$COMBO==1 & first_line$TREATMENT_DURATION < 340 & first_line$TREATMENT_DISCONTINUE > 365 & first_line$INVALID == 0 & first_line$COHORT_DEFINITION_ID %in% c(1001, 1002),]) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'First_Line_Under_Drop',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  first_result4 <- first_line %>% filter(TREATMENT_DURATION >= 340 & INVALID == 0) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'First_Line_Over',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  first_result5 <- union_all(first_line[first_line$COMBO==1 & first_line$TREATMENT_DURATION >= 340 & first_line$TREATMENT_DISCONTINUE > 180 & first_line$INVALID == 0 & first_line$COHORT_DEFINITION_ID %!in% c(1001, 1002),],
                             first_line[first_line$COMBO==1 & first_line$TREATMENT_DURATION >= 340 & first_line$TREATMENT_DISCONTINUE > 365 & first_line$INVALID == 0 & first_line$COHORT_DEFINITION_ID %in% c(1001, 1002),]) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'First_Line_Over_Drop',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  first_result <- rbind(first_result1, first_result2, first_result3, first_result4, first_result5)
  write.csv(first_result, file = file.path(outputFolder, "results/TreatmentPathways/first_duration.csv"))

  # treatment_duration (SECOND_LINE) -------------------------------------------

  # If a prescription gap exceeds one year, it is considered treatment discontinuation (exclude BPs)
  second_line_2 <- line_data %>% filter(LINE==2 & COHORT_DEFINITION_ID %!in% c(1001, 1002)) %>%
    group_by(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LAST_VISIT_DATE, LINE_START_DATE, LINE_END_DATE) %>%
    mutate(NEXT_DRUG_EXPOSURE_DATE = lag(DRUG_EXPOSURE_DATE, 1),
           NEXT_INVALID = ifelse(as.numeric(NEXT_DRUG_EXPOSURE_DATE-DRUG_EXPOSURE_DATE)<=365, 0, 1)) %>%
    filter(NEXT_INVALID == 1) %>%
    group_by(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LAST_VISIT_DATE, LINE_START_DATE, LINE_END_DATE) %>%
    filter(P_RANK==max(P_RANK)) %>% as.data.frame() %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           TREATMENT_END_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(TREATMENT_END_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-TREATMENT_END_DATE)) %>%
    select(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, LAST_DRUG_EXPOSURE_DATE, TREATMENT_END_DATE, TREATMENT_DURATION, TREATMENT_DISCONTINUE)

  # If the line changes, it is considered discontinue (only BPs)
  second_line_3 <- line_data %>% filter(LINE==2 & P_RANK==1 & SUBJECT_ID %!in% second_line_2$SUBJECT_ID) %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           TREATMENT_END_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(TREATMENT_END_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-TREATMENT_END_DATE)) %>%
    select(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, LAST_DRUG_EXPOSURE_DATE, TREATMENT_END_DATE, TREATMENT_DURATION, TREATMENT_DISCONTINUE)

  # second Line Data set
  second_line <- rbind(second_line_2, second_line_3)
  second_line_cnt <- line_data %>% left_join(second_line) %>%
    filter(LINE==2 & DRUG_EXPOSURE_DATE <= LAST_DRUG_EXPOSURE_DATE) %>%
    group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
    summarise(PRECRIPTION_CNT =  sum(DAYS_SUPPLY)) %>%
    select(COHORT_DEFINITION_ID, SUBJECT_ID, PRECRIPTION_CNT)
  second_line <- second_line %>% left_join(second_line_cnt)
  saveRDS(object = second_line, file = file.path(outputFolder, "tmpData/second_line.RDS"))

  # Duration of Treatment
  second_result1 <- second_line %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'second_Line_ALL',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  second_result2 <- second_line %>% filter(TREATMENT_DURATION < 340) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'second_Line_Under',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  second_result3 <- union_all(second_line[second_line$COMBO==1 & second_line$TREATMENT_DURATION < 340 & second_line$TREATMENT_DISCONTINUE > 180 & second_line$COHORT_DEFINITION_ID %!in% c(1001, 1002),],
                              second_line[second_line$COMBO==1 & second_line$TREATMENT_DURATION < 340 & second_line$TREATMENT_DISCONTINUE > 365 & second_line$COHORT_DEFINITION_ID %in% c(1001, 1002),]) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'second_Line_Under_Drop',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  second_result4 <- second_line %>% filter(TREATMENT_DURATION >= 340) %>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'second_Line_Over',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  second_result5 <- union_all(second_line[second_line$COMBO==1 & second_line$TREATMENT_DURATION >= 340 & second_line$TREATMENT_DISCONTINUE > 180 & second_line$COHORT_DEFINITION_ID %!in% c(1001, 1002),],
                              second_line[second_line$COMBO==1 & second_line$TREATMENT_DURATION >= 340 & second_line$TREATMENT_DISCONTINUE > 365 & second_line$COHORT_DEFINITION_ID %in% c(1001, 1002),])%>%
    group_by(COHORT_DEFINITION_ID) %>%
    summarise(INDEX = 'second_Line_Over_Drop',
            P_CNT = n_distinct(SUBJECT_ID),
            duration_median=paste0(median(TREATMENT_DURATION), "[", quantile(TREATMENT_DURATION)[2],"-", quantile(TREATMENT_DURATION)[4], "]"),
            duration_mean=paste0(round(mean(TREATMENT_DURATION),2),"(", round(sd(TREATMENT_DURATION),2), ")"),
            cnt_median=paste0(median(PRECRIPTION_CNT), "[", quantile(PRECRIPTION_CNT)[2],"-", quantile(PRECRIPTION_CNT)[4], "]"),
            cnt_mean=paste0(round(mean(PRECRIPTION_CNT),2),"(", round(sd(PRECRIPTION_CNT),2), ")"))

  second_result <- rbind(second_result1, second_result2, second_result3, second_result4, second_result5)
  write.csv(second_result, file = file.path(outputFolder, "results/TreatmentPathways/second_duration.csv"))


  # treatment_duration (THIRD_LINE) -------------------------------------------

  # If a prescription gap exceeds one year, it is considered treatment discontinuation (exclude BPs)
  third_line_2 <- line_data %>% filter(LINE==3 & COHORT_DEFINITION_ID %!in% c(1001, 1002)) %>%
    group_by(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LAST_VISIT_DATE, LINE_START_DATE, LINE_END_DATE) %>%
    mutate(NEXT_DRUG_EXPOSURE_DATE = lag(DRUG_EXPOSURE_DATE, 1),
           NEXT_INVALID = ifelse(as.numeric(NEXT_DRUG_EXPOSURE_DATE-DRUG_EXPOSURE_DATE)<=365, 0, 1)) %>%
    filter(NEXT_INVALID == 1) %>%
    group_by(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LAST_VISIT_DATE, LINE_START_DATE, LINE_END_DATE) %>%
    filter(P_RANK==max(P_RANK)) %>% as.data.frame() %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           TREATMENT_END_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(TREATMENT_END_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-TREATMENT_END_DATE)) %>%
    select(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, LAST_DRUG_EXPOSURE_DATE, TREATMENT_END_DATE, TREATMENT_DURATION, TREATMENT_DISCONTINUE)

  # If the line changes, it is considered discontinue (only BPs)
  third_line_3 <- line_data %>% filter(LINE==3 & P_RANK==1 & SUBJECT_ID %!in% third_line_2$SUBJECT_ID) %>%
    mutate(LAST_DRUG_EXPOSURE_DATE=DRUG_EXPOSURE_DATE,
           TREATMENT_END_DATE=LAST_DRUG_EXPOSURE_DATE+DURATION,
           TREATMENT_DURATION=as.numeric(TREATMENT_END_DATE-LINE_START_DATE),
           TREATMENT_DISCONTINUE=as.numeric(LAST_VISIT_DATE-TREATMENT_END_DATE)) %>%
    select(COMBO, COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, LAST_DRUG_EXPOSURE_DATE, TREATMENT_END_DATE, TREATMENT_DURATION, TREATMENT_DISCONTINUE)

  # third Line Data set
  third_line <- rbind(third_line_2, third_line_3)
  third_line_cnt <- line_data %>% left_join(third_line) %>%
    filter(LINE==3 & DRUG_EXPOSURE_DATE <= LAST_DRUG_EXPOSURE_DATE) %>%
    group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
    summarise(PRECRIPTION_CNT =  sum(DAYS_SUPPLY)) %>%
    select(COHORT_DEFINITION_ID, SUBJECT_ID, PRECRIPTION_CNT)
  third_line <- third_line %>% left_join(third_line_cnt)
  saveRDS(object = third_line, file = file.path(outputFolder, "tmpData/third_line.RDS"))

  # switching period (1st to 2nd) --------------------------------------------------------
  first_to_second <- first_line %>% left_join(second_line, by = "SUBJECT_ID", suffix = c("_FIRST", "_SECOND")) %>%
    filter(COMBO_FIRST != 1 & COHORT_DEFINITION_ID_FIRST != COHORT_DEFINITION_ID_SECOND) %>%
    select(SUBJECT_ID, COMBO_FIRST, COHORT_DEFINITION_ID_FIRST, TREATMENT_END_DATE_FIRST, TREATMENT_DURATION_FIRST,
           LINE_START_DATE_SECOND, TREATMENT_END_DATE_SECOND)

  first_to_second_swith <- first_to_second %>%
    mutate(GAP_DAYS = as.numeric(LINE_START_DATE_SECOND-TREATMENT_END_DATE_FIRST),
           INVALID = ifelse((COHORT_DEFINITION_ID_FIRST %!in% c(1001, 1002) & GAP_DAYS > 180)|(COHORT_DEFINITION_ID_FIRST %in% c(1001, 1002) & GAP_DAYS > 365), 1, 0)) %>%
    group_by(COHORT_DEFINITION_ID_FIRST) %>%
    summarise(INDEX = 'FRIST_TO_SECOND',
            P_CNT = n_distinct(SUBJECT_ID),
            gap_median=paste0(median(GAP_DAYS), "[", quantile(GAP_DAYS)[2],"-", quantile(GAP_DAYS)[4], "]"),
            gap_mean=paste0(round(mean(GAP_DAYS),2),"(", round(sd(GAP_DAYS),2), ")"),
            discontinuation = sum(INVALID))

  # switching period (2nd to 3rd) --------------------------------------------------------
  second_to_third <- second_line %>% left_join(third_line, by = "SUBJECT_ID", suffix = c("_SECOND", "_THIRD")) %>%
    filter(COMBO_SECOND != 1 & COHORT_DEFINITION_ID_SECOND != COHORT_DEFINITION_ID_THIRD) %>%
    select(SUBJECT_ID, COMBO_SECOND, COHORT_DEFINITION_ID_SECOND, TREATMENT_END_DATE_SECOND, TREATMENT_DURATION_SECOND,
           LINE_START_DATE_THIRD, TREATMENT_END_DATE_THIRD)

  second_to_third_swith <- second_to_third %>%
    mutate(GAP_DAYS = as.numeric(LINE_START_DATE_THIRD-TREATMENT_END_DATE_SECOND),
           INVALID = ifelse((COHORT_DEFINITION_ID_SECOND %!in% c(1001, 1002) & GAP_DAYS > 180)|(COHORT_DEFINITION_ID_SECOND %in% c(1001, 1002) & GAP_DAYS > 365), 1, 0)) %>%
    group_by(COHORT_DEFINITION_ID_SECOND) %>%
    summarise(INDEX = 'SECOND_TO_THIRD',
            P_CNT = n_distinct(SUBJECT_ID),
            gap_median=paste0(median(GAP_DAYS), "[", quantile(GAP_DAYS)[2],"-", quantile(GAP_DAYS)[4], "]"),
            gap_mean=paste0(round(mean(GAP_DAYS),2),"(", round(sd(GAP_DAYS),2), ")"),
            discontinuation = sum(INVALID))

  # switch_result <- rbind(first_to_second_swith, second_to_third_swith)
  write.csv(first_to_second_swith, file = file.path(outputFolder, "results/TreatmentPathways/first_to_second_swith.csv"))
  write.csv(second_to_third_swith, file = file.path(outputFolder, "results/TreatmentPathways/second_to_third_swith.csv"))


  # discontinuation (DMAB) ---------------------------------------------------
  connection <- connect(connectionDetails)
  fracture.sql <- paste("select * from (select distinct subject_id from @cohortDatabaseSchema.@cohortTable_PRESCRIPTION_EVENTS where line != 0) d,",
                        "(select person_id, condition_concept_id, concept_name, condition_start_date from @cdmDatabaseSchema.condition_occurrence cd, @cdmDatabaseSchema.concept c",
                        "where condition_concept_id in (select descendant_concept_id from @cdmDatabaseSchema.concept_ancestor",
                        "where ancestor_concept_id in (4001458, 4222001, 4053828, 4013156, 4009296, 4210437, 4013613, 4129394, 4129420, 4053654))",
                        "and condition_concept_id=concept_id) t1",
                        "where d.subject_id=t1.person_id;")
  fracture.sql <- SqlRender::render(sql = fracture.sql,
                                    cdmDatabaseSchema=cdmDatabaseSchema,
                                    cohortDatabaseSchema=cohortDatabaseSchema,
                                    cohortTable=cohortTable)

  fracture_data <- DatabaseConnector::querySql(sql = fracture.sql, connection = connection)
  first_fracture <-  fracture_data %>% select(SUBJECT_ID, CONDITION_START_DATE) %>% group_by(SUBJECT_ID) %>%
    summarise(CONDITION_START_DATE=min(CONDITION_START_DATE)) %>% as.data.frame()

  dmab_discontinuation_1 <- first_line %>% filter(COMBO==1 & TREATMENT_DISCONTINUE > 180 & COHORT_DEFINITION_ID == 1004 & INVALID==0) %>%
    mutate(OBSERVATION_END_DATE = TREATMENT_END_DATE + TREATMENT_DISCONTINUE,
           LINE=1) %>%
    select(LINE, SUBJECT_ID, TREATMENT_DURATION, TREATMENT_END_DATE, OBSERVATION_END_DATE)

  dmab_discontinuation_2 <- first_to_second %>%
    mutate(GAP_DAYS = as.numeric(LINE_START_DATE_SECOND-TREATMENT_END_DATE_FIRST)) %>%
    filter(GAP_DAYS > 180 & COHORT_DEFINITION_ID_FIRST == 1004) %>%
    mutate(OBSERVATION_END_DATE = LINE_START_DATE_SECOND,
           TREATMENT_END_DATE = TREATMENT_END_DATE_FIRST,
           TREATMENT_DURATION = TREATMENT_DURATION_FIRST,
           LINE=2) %>%
    select(LINE, SUBJECT_ID, TREATMENT_DURATION, TREATMENT_END_DATE, OBSERVATION_END_DATE)

  dmab_discontinuation <- rbind(dmab_discontinuation_1, dmab_discontinuation_2)

  dmab_fracture <- dmab_discontinuation %>% left_join(first_fracture) %>% filter(!is.na(CONDITION_START_DATE)) %>%
    mutate(DURATION = as.numeric(CONDITION_START_DATE-TREATMENT_END_DATE),
           F_VALID = ifelse(CONDITION_START_DATE<TREATMENT_END_DATE,0,
                            ifelse(CONDITION_START_DATE >= TREATMENT_END_DATE & CONDITION_START_DATE < TREATMENT_END_DATE+180,1,
                                   ifelse(CONDITION_START_DATE >= TREATMENT_END_DATE+180 & CONDITION_START_DATE < TREATMENT_END_DATE+270,2,3)))) %>%
    group_by(LINE, F_VALID) %>%
    summarise(INDEX = "DMAB",
            p_cnt=n_distinct(SUBJECT_ID),
            duration_median=paste0(median(DURATION), "[", quantile(DURATION)[2],"-", quantile(DURATION)[4], "]"),
            duration_mean=paste0(round(mean(DURATION),2),"(", round(sd(DURATION),2), ")"),)

  bp_discontinuation_1 <- first_line %>% filter(COMBO==1 & TREATMENT_DISCONTINUE > 365 & COHORT_DEFINITION_ID %in% c(1001,1002) & INVALID==0) %>%
    mutate(OBSERVATION_END_DATE = TREATMENT_END_DATE + TREATMENT_DISCONTINUE,
           LINE=1) %>%
    select(LINE, SUBJECT_ID, TREATMENT_DURATION, TREATMENT_END_DATE, OBSERVATION_END_DATE)

  bp_discontinuation_2 <- first_to_second %>%
    mutate(GAP_DAYS = as.numeric(LINE_START_DATE_SECOND-TREATMENT_END_DATE_FIRST)) %>%
    filter(GAP_DAYS > 365 & COHORT_DEFINITION_ID_FIRST %in% c(1001,1002)) %>%
    mutate(OBSERVATION_END_DATE = LINE_START_DATE_SECOND,
           TREATMENT_END_DATE = TREATMENT_END_DATE_FIRST,
           TREATMENT_DURATION = TREATMENT_DURATION_FIRST,
           LINE=2) %>%
    select(LINE, SUBJECT_ID, TREATMENT_DURATION, TREATMENT_END_DATE, OBSERVATION_END_DATE)

  bp_discontinuation <- rbind(bp_discontinuation_1, bp_discontinuation_2)

  bp_fracture <- bp_discontinuation %>% left_join(first_fracture) %>% filter(!is.na(CONDITION_START_DATE)) %>%
    mutate(DURATION = as.numeric(CONDITION_START_DATE-TREATMENT_END_DATE),
           F_VALID = ifelse(CONDITION_START_DATE<TREATMENT_END_DATE,0,
                            ifelse(CONDITION_START_DATE >= TREATMENT_END_DATE & CONDITION_START_DATE < TREATMENT_END_DATE+365,1,
                                   ifelse(CONDITION_START_DATE >= TREATMENT_END_DATE+365 & CONDITION_START_DATE < TREATMENT_END_DATE+455,2,3)))) %>%
    group_by(LINE, F_VALID) %>%
    summarise(INDEX = "BP",
            p_cnt=n_distinct(SUBJECT_ID),
            duration_median=paste0(median(DURATION), "[", quantile(DURATION)[2],"-", quantile(DURATION)[4], "]"),
            duration_mean=paste0(round(mean(DURATION),2),"(", round(sd(DURATION),2), ")"),)
  fracture_agg <- rbind(dmab_fracture, bp_fracture)
  write.csv(fracture_agg, file = file.path(outputFolder, "results/TreatmentPathways/fracture_cnt.csv"))

  # demographics ------------------------------------------------------------
  first_line_cohort <- first_line %>% select(COHORT_DEFINITION_ID, SUBJECT_ID, LINE_START_DATE, TREATMENT_END_DATE)
  colnames(first_line_cohort) <- c("COHORT_DEFINITION_ID", "SUBJECT_ID", "COHORT_START_DATE", "COHORT_END_DATE")
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste0(cohortDatabaseSchema, ".", cohortTable, "_demographics"),
                                 data = first_line_cohort,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE)

  CohortCounts <- read.csv(file.path(outputFolder, "results/TreatmentPathways/CohortCounts.csv"))
  
  covariates_1  <-  FeatureExtraction::createDefaultCovariateSettings()

  
  for (id in c(1001:1006)){
    valid_cohort <- length(CohortCounts[CohortCounts$cohortDefinitionId==id,]$personCount)
    if(valid_cohort==1){
      covariateData_1 <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                                             cohortTable = paste0(cohortTable, "_demographics"),
                                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                                             cohortId = id,
                                                             rowIdField = "subject_id",
                                                             aggregated = TRUE,
                                                             covariateSettings = covariates_1)
      table1 <- FeatureExtraction::createTable1(covariateData1 = covariateData_1, cohortId1 = id)
      write.csv(table1, file.path(outputFolder, paste0("results/TreatmentPathways/table1_", id, ".csv")))
      FeatureExtraction::saveCovariateData(covariateData = covariateData_1, file = file.path(outputFolder, paste0("tmpData/covariate_1_", id, ".zip")))
      
      sql <- SqlRender::loadRenderTranslateSql("customCovariates.sql",
                                               "ODTP",
                                               dbms = connectionDetails$dbms,
                                               oracleTempSchema = oracleTempSchema,
                                               cdm_database_schema = cdmDatabaseSchema,
                                               cohort_table = paste0(cohortDatabaseSchema,".", cohortTable, "_demographics"),
                                               cohort_id = id)
            # Retrieve the covariate:
      covariates_2 <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
      table_2 <- covariates_2 %>% group_by(covariateId) %>% summarise(sumValue=sum(covariateValue)) %>% as.data.frame()
      write.csv(table_2, file.path(outputFolder, paste0("results/TreatmentPathways/table2_", id, ".csv")))
    }
    else {
      print(paste(id, "is empty."))
    }
  }
  print("Done!")
  disconnect(connection)
}


