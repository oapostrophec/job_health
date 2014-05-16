#This table is dumb
get_country_data <- function(id){
  selects = paste(" select country_code, excluded", sep="")

  from = paste(" from builder_job_countries", sep="")
  where = paste(" where job_id=", id, sep="")
  
  query_items = paste(selects, from, where, sep="")
  return(query_items)
}