
# select worker_ui_worksets.job_id,
# count(distinct worker_ui_worksets.builder_worker_id) as num_exit_surveys,
# AVG(worker_ui_exit_surveys.payrate_relativity) as payrate  
# from worker_ui_exit_surveys 
# join worker_ui_worksets on worker_ui_worksets.id = worker_ui_exit_surveys.workset_id
# where worker_ui_worksets.job_id = 404391
# group by worker_ui_worksets.job_id


payrate_satisfaction_query <- function(job_id) {
  part1 ="select worker_ui_worksets.job_id,
  count(distinct worker_ui_worksets.builder_worker_id) as num_exit_surveys,
  AVG(worker_ui_exit_surveys.payrate_relativity) as payrate  
  from worker_ui_exit_surveys 
  join worker_ui_worksets on worker_ui_worksets.id = worker_ui_exit_surveys.workset_id
  where worker_ui_worksets.job_id = "
  id_to_paste =job_id
  part2 = " group by worker_ui_worksets.job_id"
  query = paste0(part1, id_to_paste, part2)
  return(query)
}