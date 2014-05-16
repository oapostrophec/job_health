get_channel_data <- function(id){

    selects = paste("select job_id, channel_name, external_id", sep="")
    from = paste(" from builder_job_channels", sep="")
    where = paste(" where job_id=", id, sep="")
    
    query_items = paste(selects, from, where, sep="")
    return(query_items)
}


