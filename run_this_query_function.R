############# run any query with connection, query and file parts

run_this_query <- function(db_part, query, file_to_write) {
  command = paste(db_part, '"', query, '"',
                  ' > ', file_to_write)
  system(command)
  data_back = read.csv(file_to_write)
  return(data_back)
}