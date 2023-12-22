DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, drug_concept_id, cohort_start_date, cohort_end_date, days_supply, duration)
select @target_cohort_id as cohort_definition_id, d.person_id, d.drug_concept_id, d.drug_exposure_start_date, d.drug_exposure_end_date, d.days_supply, d.duration
from (
  select de.*, 90 as 'duration'
  FROM @cdm_database_schema.DRUG_EXPOSURE de  
  WHERE days_supply >=1 and de.drug_concept_id in (21604154, 40173414)
union all
select de.*, 365 as 'duration'
FROM @cdm_database_schema.DRUG_EXPOSURE de
left Join (
SELECT co.person_id, min(condition_start_date) as condition_start_date
FROM @cdm_database_schema.CONDITION_OCCURRENCE co
WHERE co.condition_concept_id in (select distinct I.concept_id 
								  FROM (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (78097,76914)
										UNION  select c.concept_id from @cdm_database_schema.CONCEPT c join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
										and ca.ancestor_concept_id in (78097,76914) and c.invalid_reason is null) I)
group by co.person_id
)co on de.person_id=co.person_id and condition_start_date>drug_exposure_start_date and condition_start_date>drug_exposure_end_date
WHERE de.drug_concept_id in (21604156, 41359781, 21118274) and de.days_supply >= 1 
  ) d;
