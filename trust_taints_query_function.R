# select worker_id, golden_trust from builder_worksets
# where tainted= 'true' and flagged_at IS NULL and rejected_at IS NULL
# and job_id = 378758

trust_taints_query <- function(job_id) {
  part1 = "select worker_id, golden_trust from builder_worksets
where tainted= 'true' and flagged_at IS NULL and rejected_at IS NULL
and job_id = "
  query = paste0(part1, job_id)
  return(query)
}