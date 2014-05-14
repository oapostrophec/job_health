
get_job_data <- function(id){
	psql =  
	 paste('psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c ') 

  selects = paste('select id, user_id, title, minimum_account_age_seconds, \'\\"\' || \\"minimum_requirements\\" || \'\\"\' as minimum_requirements, require_worker_login', sep="")

  from = paste(" from builder_jobs", sep="")
  where = paste(" where id=", id, sep="")
  file = paste("/tmp/job_health/jobs",id,".csv",sep="")

	command = paste(psql, '"', selects, from, where, '"'," > ", '\'', file, '\'', sep="")
  print("get_job_data query:")
  print(command)
  system(command)
  data = read.csv(file,stringsAsFactors=FALSE)
  print("Did you make it here? Job Data, line 18")
	data$minimum_requirements = 
	  str_replace_all(data$minimum_requirements, ",", "\n")
  return(data)
}