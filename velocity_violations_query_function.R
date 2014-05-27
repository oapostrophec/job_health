# select worker_id, velocity, "limit", violation from worker_ui_rate_limits
# where job_id = 387320


velocity_violations_query <- function(job_id) { # potentailly skill and country arguments here
  query_start = "select worker_id, velocity, \\\"limit\\\", violation from worker_ui_rate_limits
where job_id = "
  query = paste0(query_start,job_id)
  return(query)
}