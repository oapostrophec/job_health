get_judgment_data <- function(id){

    selects = paste("select id, trust, worker_id, unit_id, started_at, rejected,
                     missed, contested, tainted, ip, country, city, golden, webhook_sent_at", 
                     sep="")
    from = paste("from judgments", sep="")
    where = paste("where job_id=", id, sep="")

    query_items = paste(selects, from, where, sep="")
    
    return(query_items)
}