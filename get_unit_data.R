get_unit_data <- function(id){  
  selects = paste('select id, state, agreement, judgments_count, missed_count, contested_count', sep="")
  from = paste(" from builder_units", sep="")
  where = paste(" where job_id=", id, sep="")
  query_items = paste(selects, from, where, sep="")
  return(query_items)
}