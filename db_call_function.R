################### the database call
################## to prepend queries
require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('devtools')
require('stringr')
require('reshape2')
require('stringr')
library('XML')
library('rjson')


db_call <- function(connection_file = "builder_readonly") {
  psql = paste('psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439') 
  export_options = "-A -F',' -c "
  db_call_to_prepend = paste(psql, export_options)
  return(db_call_to_prepend)
}