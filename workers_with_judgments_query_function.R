# select worker_id, judgments_count from builder_worksets
# where job_id = 378758
# and judgments_count > 0
# order by judgments_count

workers_with_judgments_query <- function(job_id) {
  part1 = "select worker_id, judgments_count from builder_worksets
where job_id = "
  part2 = "
and judgments_count > 0
order by judgments_count"
  query = paste0(part1, job_id, part2)
  return(query)
}
