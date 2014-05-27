# select worker_id, flag_reason from builder_worksets
# where flag_reason IS NOT NULL and flag_reason LIKE '%acceptable_distribution%'
# and job_id = 378758

answer_flags_query <- function(job_id) {
  part1 = "select worker_id, flag_reason from builder_worksets
where flag_reason IS NOT NULL and flag_reason LIKE '%acceptable_distribution%'
and job_id = "
  query = paste0(part1, job_id)
  return(query)
}