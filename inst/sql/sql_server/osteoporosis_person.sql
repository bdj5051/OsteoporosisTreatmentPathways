with person_osteoporosis as (
	select co.person_id, p.year_of_birth, min(condition_start_date) as condition_start_date 
	from @cdm_database_schema.condition_occurrence co, @cdm_database_schema.person p
	where co.condition_concept_id in (4010333,80502,81390,77365) and p.person_id = co.person_id and p.gender_concept_id = 8532
	group by co.person_id, p.year_of_birth
)
select *, year(condition_start_date)-year_of_birth as age from person_osteoporosis;