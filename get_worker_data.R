
get_worker_data <- function(id){
	builder_ro = pgConnectionInfo("builder_readonly")

    psql = paste("psql", builder_ro["database"], "-U", builder_ro["user"], "-h", builder_ro["host"], 
    	          "-p", builder_ro["port"], 
      	     	  "-A -F\',\' -c ")

    selects = paste("select worker_id, agreement, judgments_count, missed_count, golds_count, 
    	            flagged_at, rejected_at, work_phase, tainted", sep="")

    from = paste("from worksets", sep="")
    where = paste("where job_id=", id, sep="")
    file = paste("/tmp/job_health/worksets",id,".csv",sep="")

    command = paste(psql, '"', selects, from, where, '"', ">", file, sep="")
    system(command)
    data = read.csv(file,stringsAsFactors=FALSE)
    return(data)
}
