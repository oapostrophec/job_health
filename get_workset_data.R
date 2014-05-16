
get_workset_data <- function(id){
  
  selects = paste('select job_id, worker_id, id, judgments_count', sep="")
  from = paste(" from builder_worksets", sep="")
  where = paste(" where job_id=", id, sep="")
  
  query_item = paste(selects, from, where, sep="")
  
  return(query_item)
}
