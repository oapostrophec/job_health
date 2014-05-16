  ## Job Heart Monitor
  ## May 11, 2014
  ## 5 min job health
  
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
  
  source('get_job_data.R')
  #source('get_judgment_data.R')
  #source('get_country_data.R')
  source('get_channel_data.R')
  #source('get_worker_data.R')
  source('get_workset_data.R')
  source('db_call_function.R')
  source('run_this_query_function.R')
  source('work_available_query_function.R')
  source('payrate_satisfaction_query_function.R')
  source('dropout_rate_query_function.R')
  
  temp_dir = "/tmp/job_health"
  
  system(paste('mkdir -p',temp_dir))
  
  options(stringsAsFactors = F)
  options(shiny.maxRequestSize=150*1024^2)
  options(scipen=999)
  
  db_call = db_call(connection_file = "builder_readonly")
  
  shinyServer(function(input, output){
    
    output$renderLogo <- renderText({
      image_path = "http://cf-public-view.s3.amazonaws.com/coolstuff/cf_logo_blue.png"
      html_image = paste("<img class=\"img-circle\" src=", image_path, " width=\"55%\"/>", sep="")
      paste(html_image)
    }) 
    
    output$jobSummary <- renderText({
      #if(input$get_job == 0 && input$get_email == 0){
      if(input$get_job ==0){
        return(NULL)
      }else{
        output="Job Summary Info Here:"
      }
      
    })
    
    output$accountSummary <- renderText({
      #if(input$get_job == 0 && input$get_email == 0){
      if(input$get_job == 0){
        return(NULL)
      }else{
        output="Account Summary Info Here:"
      }
      
    })
    
    pull_channel_data <- reactive({
      #if(input$get_job == 0 && input$get_email == 0){
      if(input$get_job == 0){
        return(NULL)
      #}else if(input$get_job != 0){
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          data = get_channel_data(job_id) 
        }
      } 
    })
    
    output$channelData <- renderTable({
      if(input$get_job == 0){
        return(NULL)
        #}else if(input$get_job != 0){
      }else{
        table = pull_channel_data()
        table
      }
    })
    
    pull_job_data <- reactive({
      #if(input$get_job == 0 && input$get_email == 0){
      if(input$get_job == 0){
        return(NULL)
        #}else if(input$get_job != 0){
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          data = get_job_data(job_id) 
          print(head(data))
          data
        }
      }
    })
    
    output$jobData <- renderTable({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          table = pull_job_data()
          table
        } 
      }
    })
    
    pull_workset_data <- reactive({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          data = get_workset_data(job_id)
          print(head(data))
          data
        } 
      }
    })
    
    output$worksetData <- renderTable({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          table = pull_workset_data()
          table
        } 
      }
      
    })
    
    pull_work_available <- reactive({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          db = db_call
          query = work_available_query()
          file = paste0(temp_dir,"/",
            "work_available", "_",
                        format(Sys.time(), "%b_%d_%X_%Y"),
                        ".csv")
          data = run_this_query(db, query, file)
          print(head(data))
          data
        } 
      }
    })
    
    output$numJobsAvailable <-  renderTable({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          table = pull_work_available()
          print(summary(table))
          head(table)
        } 
      }
      
    })
    
    pull_payrate_satisfaction <- reactive({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        } else {
          db = db_call
          query = payrate_satisfaction_query(job_id)
          file = paste0(temp_dir,"/",
                        "payrate_satisfaction", "_",
                        format(Sys.time(), "%b_%d_%X_%Y"),
                        ".csv")
          data = run_this_query(db, query, file)
          print(head(data))
          data
        } 
      }
    })
    
    output$payrateSatisfaction <-  renderTable({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          table = pull_payrate_satisfaction()
          print(summary(table))
          head(table)
        } 
      }
      
    })
    
    pull_dropout_rate <- reactive({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        } else {
          db = db_call
          query = dropout_rate_query(job_id)
          file = paste0(temp_dir,"/",
                        "payrate_satisfaction", "_",
                        format(Sys.time(), "%b_%d_%X_%Y"),
                        ".csv")
          data = run_this_query(db, query, file)
          print(head(data))
          data
        } 
      }
    })
    
    output$dropoutRate <-  renderTable({
      if(input$get_job == 0){
        return(NULL)
      }else{
        job_id = input$job_id
        if(job_id == 0){
          return(NULL)
        }else{
          table = pull_dropout_rate()
          print(summary(table))
          head(table)
        } 
      }
      
    })
    
  })