# select count(*) from builder_worksets
# where judgments_count = 0
# and job_id = 388220


workers_with_no_judgments_query <- function(job_id) {
  part1 = "select count(*) from builder_worksets
where judgments_count = 0
and job_id = "
  query = paste0(part1, job_id)
  return(query)
}

