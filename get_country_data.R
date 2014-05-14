#This table is dumb
get_country_data <- function(id){
	builder_ro = pgConnectionInfo("builder_readonly")

    psql = paste("psql", builder_ro["database"], "-U", builder_ro["user"], "-h", builder_ro["host"], 
    	          "-p", builder_ro["port"], 
      	     	  "-A -F\',\' -c ")

    selects = paste("select country_code, excluded", sep="")

    from = paste("from job_countries", sep="")
    where = paste("where job_id=", id, sep="")
    file = paste("/tmp/job_health/job_countries",id,".csv",sep="")

    command = paste(psql, '"', selects, from, where, '"', ">", file, sep="")
    system(command)
    data = read.csv(file,stringsAsFactors=FALSE)
    return(data)
}