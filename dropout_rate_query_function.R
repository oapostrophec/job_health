# select builder_worker_id,
# sum(CASE when state = 'completed' then 1
#     else 0
#     END) as completed_assignments,
# sum(CASE when (state = 'completed' and worker_mode = 'quiz') then 1
#     else 0
#     END) as quiz_completed,
# sum(CASE when (state = 'completed' and worker_mode = 'work') then 1
#     else 0
#     END) as work_completed,
# sum(CASE when (quit_at IS NOT NULL OR abandoned_at IS NOT NULL) then 1
#     else 0
#     END) as assignments_gave_up,
# sum(CASE when expired_at IS NOT NULL then 1
#     else 0
#     END) assignments_expired,
# count(*) as all_assignments
# from worker_ui_assignments
# where job_id = 152180
# group by builder_worker_id 


dropout_rate_query <- function(job_id) {
  part1 ="select builder_worker_id,
  sum(CASE when state = 'completed' then 1
      else 0
      END) as completed_assignments,
  sum(CASE when (state = 'completed' and worker_mode = 'quiz') then 1
      else 0
      END) as quiz_completed,
  sum(CASE when (state = 'completed' and worker_mode = 'work') then 1
      else 0
      END) as work_completed,
  sum(CASE when (quit_at IS NOT NULL OR abandoned_at IS NOT NULL) then 1
      else 0
      END) as assignments_gave_up,
  sum(CASE when expired_at IS NOT NULL then 1
      else 0
      END) assignments_expired,
  count(*) as all_assignments
  from worker_ui_assignments
  where job_id = "
  id_to_paste =job_id
  part2 = " group by builder_worker_id"
  query = paste0(part1, id_to_paste, part2)
  return(query)
}