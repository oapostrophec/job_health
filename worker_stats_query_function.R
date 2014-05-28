# select worker_id,
# CASE when judgments_count = 0 then 1
# else 0
# END as no_judgments,
# CASE when judgments_count > 0 then judgments_count
# else 0
# END as num_judgments,
# golden_trust,
# CASE
# when tainted= 'true' and flagged_at IS NULL and rejected_at IS NULL then 1
# else 0
# END as turst_taint,
# tainted,
# flagged_at, 
# rejected_at, 
# flag_reason,
# CASE when flag_reason IS NOT NULL and flag_reason LIKE '%acceptable_distribution%' then 1
# else 0
# END as answer_distribution_flags,
# id,
# golds_count, 
# missed_count,
# forgiven_count, 
# judgments_count
# from builder_worksets
# where job_id = 378758

worker_stats_query <- function(job_id) {
  part1 = "select worker_id,
CASE when judgments_count = 0 then 1
else 0
END as no_judgments,
CASE when judgments_count > 0 then judgments_count
else 0
END as num_judgments,
golden_trust,
CASE
when tainted= 'true' and flagged_at IS NULL and rejected_at IS NULL then 1
else 0
END as trust_taint,
tainted,
flagged_at, 
rejected_at, 
flag_reason,
CASE when flag_reason IS NOT NULL and flag_reason LIKE '%acceptable_distribution%' then 1
else 0
END as answer_distribution_flags,
id,
golds_count, 
missed_count,
forgiven_count, 
judgments_count
from builder_worksets
where job_id = "
  query = paste0(part1, job_id)
  return(query)
}
