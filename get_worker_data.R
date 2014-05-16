
get_worker_data <- function(id){

    selects = paste("select worker_id, agreement, judgments_count, missed_count, golds_count, 
    	            flagged_at, rejected_at, work_phase, tainted", sep="")

    from = paste("from worksets", sep="")
    where = paste("where job_id=", id, sep="")

    
    query_items = paste(selects, from, where, sep="")
    return(query_items)
}
