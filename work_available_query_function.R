########### query to get work available

# select builder_jobs.id, 
# max(builder_jobs.minimum_requirements),
# max(builder_jobs.payment_cents) as cents_per_task,
# max(payment_cents::float/units_per_assignment) as cents_per_unit,
# count(builder_worksets.*) as active_workers,
# CASE when max(minimum_requirements) like '%"level_3_contributors"%' then 'level_3'
# when max(minimum_requirements) like '%"level_2_contributors"%' then 'level_2'
# when max(minimum_requirements) like '%"level_1_contributors"%' then 'level_1'
# when max(minimum_requirements) IS NULL 
# OR max(minimum_requirements) = 'null'
# OR max(minimum_requirements) = '{}' 
# OR max(minimum_requirements) like '%"skill_scores":{}%' then 'none'
# else 'some other crowd'
# END as skills
# from builder_jobs 
# join builder_worksets on builder_jobs.id = builder_worksets.job_id
# where builder_jobs.state = 2
# and builder_worksets.updated_at > GETDATE() - INTERVAL '6 hours' 
# group by builder_jobs.id

work_available_query <- function() { # potentailly skill and country arguments here
  query = "select builder_jobs.id, 
     max(builder_jobs.payment_cents) as cents_per_task,
     max(payment_cents::float/units_per_assignment) as cents_per_unit,
     count(builder_worksets.*) as active_workers,
     CASE when max(minimum_requirements) like '%\"level_3_contributors\"%' then 'level_3'
     when max(minimum_requirements) like '%\"level_2_contributors\"%' then 'level_2'
     when max(minimum_requirements) like '%\"level_1_contributors\"%' then 'level_1'
     when max(minimum_requirements) IS NULL 
     OR max(minimum_requirements) = 'null'
     OR max(minimum_requirements) = '{}' 
   OR max(minimum_requirements) like '%\"skill_scores\":{}%' then 'none'
   else 'some other crowd'
   END as skills
   from builder_jobs 
   join builder_worksets on builder_jobs.id = builder_worksets.job_id
   where builder_jobs.state = 2
   and builder_worksets.updated_at > GETDATE() - INTERVAL '6 hours' 
   group by builder_jobs.id"
  return(query)
}