
get_workset_data <- function(id){
  psql =  
    paste('psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c ') 
  
  selects = paste('select job_id, worker_id, id, judgments_count', sep="")
  
  from = paste(" from builder_worksets", sep="")
  where = paste(" where job_id=", id, sep="")
  file = paste("/tmp/job_health/worksets",id,".csv",sep="")
  
  command = paste(psql, '"', selects, from, where, '"'," > ", '\'', file, '\'', sep="")
  print("get_workset_data query:")
  print(command)
  system(command)
  data = read.csv(file,stringsAsFactors=FALSE)
  
  return(data)
}
