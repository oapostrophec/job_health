# select worker_id, tainted, flagged_at, rejected_at, flag_reason, golden_trust from builder_worksets
# where tainted= 'true'

all_taints_query <- function(job_id) {
  part1 = "select worker_id, tainted, flagged_at, rejected_at, flag_reason, golden_trust from builder_worksets
where tainted= 'true' 
and job_id = "
  query = paste0(part1, job_id)
  return(query)
}