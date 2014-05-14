get_judgment_data <- function(id){
	builder_ro = pgConnectionInfo("builder_readonly")

    psql = paste("psql", builder_ro["database"], "-U", builder_ro["user"], "-h", builder_ro["host"], 
    	          "-p", builder_ro["port"], 
      	     	  "-A -F\',\' -c ")

    selects = paste("select id, trust, worker_id, unit_id, started_at, rejected,
                     missed, contested, tainted, ip, country, city, golden, webhook_sent_at", 
                     sep="")

    from = paste("from judgments", sep="")
    where = paste("where job_id=", id, sep="")
    file = paste("/tmp/job_health/judgments",id,".csv",sep="")

    command = paste(psql, '"', selects, from, where, '"', ">", file, sep="")
    system(command)
    data = read.csv(file,stringsAsFactors=FALSE)
    return(data)
}