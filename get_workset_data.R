
get_workset_data <- function(id){
  
  selects = paste('select job_id, worker_id, id, golden_trust, golds_count, missed_count, forgiven_count, judgments_count, flagged_at, rejected_at, tainted', sep="")
  from = paste(" from builder_worksets", sep="")
  where = paste(" where job_id=", id, sep="")
  
  query_item = paste(selects, from, where, sep="")
  
  return(query_item)
}
