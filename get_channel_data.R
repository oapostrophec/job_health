get_channel_data <- function(id){

  psql =  
    paste('psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c ') 

    selects = paste("select job_id, channel_name, external_id", sep="")

    from = paste(" from builder_job_channels", sep="")
    where = paste(" where job_id=", id, sep="")
    file = paste("/tmp/job_health/job_channels",id,".csv",sep="")

    command = paste(psql, '"', selects, from, where, '"', ">", '\'', file, '\'', sep="")
    print("get_channel_data query")
    print(command)
    system(command)
    #file=paste('\'', file,'\'', sep="")
    data = read.csv(file, stringsAsFactors=FALSE)
    #print(head(data))
    return(data)
}


