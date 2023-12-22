-- BP PO
DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, drug_concept_id, cohort_start_date, cohort_end_date, days_supply, duration)
select @target_cohort_id as cohort_definition_id, d.person_id, d.drug_concept_id, d.drug_exposure_start_date, d.drug_exposure_end_date, d.days_supply, d.duration
from (
  select de.*, 7 as 'duration'
  FROM @cdm_database_schema.DRUG_EXPOSURE de
  WHERE de.drug_concept_id in (21604155,40174494,42943685,21604152,40173612) and days_supply >= 4
union all
  select de.*, 30 as 'duration'
  FROM @cdm_database_schema.DRUG_EXPOSURE de
  WHERE de.drug_concept_id in (40174486,40174364) and days_supply >= 1
  ) d;
