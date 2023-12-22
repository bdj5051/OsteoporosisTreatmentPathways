select event_cohort_index, subject_id, CAST(cohort_start_date AS DATETIME) AS cohort_start_date, CAST(cohort_end_date AS DATETIME) AS cohort_end_date
INTO #event_cohort_eras
FROM (
	SELECT ec.cohort_index AS event_cohort_index,
	  e.subject_id,
	  e.cohort_start_date,
	  dateadd(d, 1, e.cohort_end_date) as cohort_end_date
	FROM @cohortDatabaseSchema.@cohortTable e
	  JOIN (SELECT 1005 AS cohort_definition_id, 0 AS cohort_index 
			UNION ALL 
			SELECT 1004 AS cohort_definition_id, 1 AS cohort_index 
			UNION ALL 
			SELECT 1003 AS cohort_definition_id, 2 AS cohort_index
			UNION ALL 
			SELECT 1002 AS cohort_definition_id, 3 AS cohort_index 
			UNION ALL 
			SELECT 1001 AS cohort_definition_id, 4 AS cohort_index 
			) ec 
	  ON e.cohort_definition_id = ec.cohort_definition_id
	  JOIN @cohortDatabaseSchema.@cohortTable t ON t.cohort_start_date <= e.cohort_start_date AND e.cohort_start_date <= t.cohort_end_date AND t.subject_id = e.subject_id
	WHERE t.cohort_definition_id = @cohortId
) RE;


WITH
cohort_dates AS (
	SELECT DISTINCT subject_id, cohort_date
	FROM (
		  SELECT subject_id, cohort_start_date cohort_date FROM #event_cohort_eras
		  UNION
		  SELECT subject_id,cohort_end_date cohort_date FROM #event_cohort_eras
		  ) all_dates
),
time_periods AS (
	SELECT subject_id, cohort_date, LEAD(cohort_date,1) over (PARTITION BY subject_id ORDER BY cohort_date ASC) next_cohort_date
	FROM cohort_dates
	GROUP BY subject_id, cohort_date

),
events AS (
	SELECT tp.subject_id, event_cohort_index, cohort_date cohort_start_date, next_cohort_date cohort_end_date
	FROM time_periods tp
	LEFT JOIN #event_cohort_eras e ON e.subject_id = tp.subject_id
	WHERE (e.cohort_start_date <= tp.cohort_date AND e.cohort_end_date >= tp.next_cohort_date)
)
SELECT cast(SUM(POWER(cast(2 as bigint), e.event_cohort_index)) as bigint) as combo_id,  subject_id , cohort_start_date, cohort_end_date
into #combo_events
FROM events e
GROUP BY subject_id, cohort_start_date, cohort_end_date;

/*
* Remove repetitive events (e.g. A-A-A into A)
*/

SELECT
  CAST(ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY cohort_start_date) AS INT) ordinal,
  CAST(combo_id AS BIGINT) combo_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
INTO #non_repetitive_events
FROM (
  SELECT
    combo_id, subject_id, cohort_start_date, cohort_end_date,
    CASE WHEN (combo_id = LAG(combo_id) OVER (PARTITION BY subject_id ORDER BY subject_id, cohort_start_date ASC))
      THEN 1
      ELSE 0
    END repetitive_event, 
		case when ROW_NUMBER() OVER (PARTITION BY subject_id, CAST(combo_id AS BIGINT) ORDER BY cohort_start_date) > 1 then 1 else 0 end is_repeat
  FROM #combo_events
) AS marked_repetitive_events
WHERE repetitive_event = 0;

/*
* Persist results
*/
DELETE FROM @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_events where pathway_analysis_generation_id = @generationId;
INSERT INTO @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_events (pathway_analysis_generation_id, target_cohort_id, subject_id, ordinal, combo_id, cohort_start_date, cohort_end_date)
SELECT
  @generationId as pathway_analysis_generation_id,
  @cohortId as target_cohort_id,
  subject_id,
  ordinal,
  combo_id,
  cohort_start_date,
  cohort_end_date
FROM #non_repetitive_events
WHERE 1 = 1 AND ordinal <= 3;

DELETE FROM @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_stats where pathway_analysis_generation_id = @generationId;
INSERT INTO @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_stats (pathway_analysis_generation_id, target_cohort_id, target_cohort_count, pathways_count)
SELECT
  @generationId as pathway_analysis_generation_id,
  CAST(@cohortId AS INT) AS target_cohort_id,
  CAST(target_count.cnt AS BIGINT) AS target_cohort_count,
  CAST(pathway_count.cnt AS BIGINT) AS pathways_count
FROM (
  SELECT CAST(COUNT_BIG(*) as BIGINT) cnt
  FROM @cohortDatabaseSchema.@cohortTable
  WHERE cohort_definition_id = @cohortId
) target_count,
(
  SELECT CAST(COUNT_BIG(DISTINCT subject_id) as BIGINT) cnt
  FROM @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_events
  WHERE pathway_analysis_generation_id = @generationId
  AND target_cohort_id = @cohortId
) pathway_count;




DELETE FROM @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_paths where pathway_analysis_generation_id = @generationId;
INSERT INTO @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_paths (pathway_analysis_generation_id, target_cohort_id, step_1, step_2, step_3, step_4, step_5, count_value)
select pathway_analysis_generation_id, target_cohort_id,
	step_1, step_2, step_3, step_4, step_5,
  count_big(subject_id) as count_value
from
(
  select e.pathway_analysis_generation_id, e.target_cohort_id, e.subject_id,
    MAX(case when ordinal = 1 then combo_id end) as step_1,
    MAX(case when ordinal = 2 then combo_id end) as step_2,
    MAX(case when ordinal = 3 then combo_id end) as step_3,
    MAX(case when ordinal = 4 then combo_id end) as step_4,
    MAX(case when ordinal = 5 then combo_id end) as step_5
  from @cohortDatabaseSchema.@cohortTable_Osteoporosis_pathway_analysis_events e
  WHERE e.pathway_analysis_generation_id = @generationId
	GROUP BY e.pathway_analysis_generation_id, e.target_cohort_id, e.subject_id
) t1
group by pathway_analysis_generation_id, target_cohort_id, 
	step_1, step_2, step_3, step_4, step_5
;

TRUNCATE TABLE #non_repetitive_events;
DROP TABLE #non_repetitive_events;

TRUNCATE TABLE #combo_events;
DROP TABLE #combo_events;

TRUNCATE TABLE #event_cohort_eras;
DROP TABLE #event_cohort_eras;
