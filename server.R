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
require('rCharts')

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

auth_key = "5b7d73e5e7eb06556f12b45f87b013fc419f45f2"

db_call = db_call(connection_file = "builder_readonly")

shinyServer(function(input, output){
  
  output$renderLogo <- renderText({
    image_path = "http://cf-public-view.s3.amazonaws.com/coolstuff/cf_logo_blue.png"
    html_image = paste("<img class=\"img-circle\" src=", image_path, " width=\"55%\"/>", sep="")
    paste(html_image)
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
      if (job_id == 0) {
        return(NULL)
      }else{
        print("in pull_workset_data")
        db = db_call
        query = get_workset_data(job_id)
        file = paste0(temp_dir,"/",
                      "workset", "_", job_id, "_",
                      format(Sys.time(), "%b_%d_%X_%Y"),
                      ".csv")
        data = run_this_query(db, query, file)
        print("Workset server Line 132")
        print(names(data))
        print(head(data))
        data
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
  
  output$fiveNumWorkerTable <-  renderTable({
    if(input$get_job == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      if(job_id == 0){
        return(NULL)
      }else{
        # total_available = 
        print(summary(table))
        head(table)
        return(1)
      } 
    }
    
  })
  
  output$throughput_bar <- renderChart({
    if (input$get_job == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      responses_table_transformed = data.frame(group=c("maxed out", "working",
                                                       "tainted","checked_out", "not_in_yet"),
                                               y = c(12,34,32,45,50), # these are the numbers found in groups
                                               x=rep("",times=5), # this is a fake grouping variable
                                               preserve_order = c(5,4,3,2,1),
                                               info = c("<b>Maxed Out</b><br>Count: 12<br>Max work set to: max work<br>Max work according to # Golds: max golds",
                                                        "Could still be <b>working</b> for all we know",
                                                        "FAIL",
                                                        "Never even started. <u>underline</u>",
                                                        "Are they even real?"), # this is a vector of html vars describing tooltips
                                               click_action = c("maxed_out_div",
                                                                "working_div",
                                                                "tainted_div",
                                                                "checked_out_div",
                                                                "not_in_yet_div") # now this is the trickiest part - we'll have to figure out shiny interactions
                                               # these will open links for now
      )
      
      
      data_list = lapply(split(responses_table_transformed, responses_table_transformed$group),
                         function(x) {
                           res <- lapply(split(x, rownames(x)), as.list)
                           names(res) <- NULL
                           return(res)
                         }
      )
      
      h1 <- rCharts::Highcharts$new()
      invisible(sapply(data_list, function(x) {
        h1$series(data = x, type = "bar", name = x[[1]]$group)
      }
      ))
      
      
      h1$plotOptions(
        series = list(
          stacking = "normal",
          pointWidth = 300,
          cursor = 'pointer',
          events = list(
            # click = "#! function() { alert('You just clicked the graph'); } !#") # simplest test
            # this.options.data[0] then column name to access data
            # click = "#! function() { window.open(this.options.data[0].click_action); } !#")
            #
            # hide all elements, then show the one corresponding to the bar
            click = "#! function() { 
            var my_divs = document.getElementsByClassName('bar_divs');
            for (var i = 0; i < my_divs.length; i++) {
              my_divs[i].style.display= 'none';
            }
            document.getElementById(this.options.data[0].click_action).style.display='block'; } !#")
        )
      )
      
      
      h1$tooltip(useHTML = T, 
                 formatter = "#! function() { return('<b>' +  this.point.y + '</b><br>' + this.point.info )} !#")
      
      h1$addParams(dom = 'throughput_bar')
      print(h1)
      
    }
    
  })
  
  get_job_settings_from_json <- reactive({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      command = paste0("curl https://api.crowdflower.com/v1/jobs/",
                       input$job_id,
                       ".json?key=",
                       auth_key)
      json_get = paste(system(command, intern=T), collapse="")
      parsed_json = fromJSON(json_str = json_get)
      return(parsed_json)
    }
  })
  
  get_max_setting_correct <- reactive({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      
      
    }
  })
  
  get_max_setting <- reactive({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if (is.null(get_job_settings_from_json()$max_judgments_per_worker) && 
            is.null(get_job_settings_from_json()$max_judgments_per_ip)) {
        this_max = Inf
      } else {
        this_max = min(get_job_settings_from_json()$max_judgments_per_worker,
                       get_job_settings_from_json()$max_judgments_per_ip)
      }
      return(this_max)
    }
  })
  
  
  get_max_setting_correct <- reactive({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_json = get_job_settings_from_json()
      print(job_json)
      num_golds = as.numeric(job_json$golds_count) # TODO for now, just pull the setting. get real number later
      units_per_task = as.numeric(job_json$units_per_assignment)
      gold_per_task = as.numeric(job_json$gold_per_assignment)
      golds_in_quiz_mode = as.numeric(job_json$options$after_gold)
      print(class(golds_in_quiz_mode))
      if (gold_per_task > 0) {
        max_correct = floor(((num_golds-golds_in_quiz_mode) / gold_per_task * units_per_task))
      } else {
        max_correct = Inf
      }
      return(max_correct)
    }
  })
  
  get_num_maxed_out <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      return(12)
    }
  })
  
  output$maxed_out_summary <- renderText({
    if (input$get_job == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      string = "123"
      paste(string)
    }
  })
  
  output$maxed_out_summary1 <- renderText({
    if (input$get_job == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      num_maxed_out = get_num_maxed_out()
      max_setting = get_max_setting()
      max_setting_correct = get_max_setting_correct()
      overview = paste0(num_maxed_out," workers have maxed out")
      comment = "(This means that they cannot do any more work)"
      max_work_line = paste("Max work per worker is set to:",max_setting)
      max_possible_line = paste("This job's params would allow for a work limit of:",max_setting_correct)
      rec_line = "The gist is:"
      if (max_setting == Inf) {
        if (max_setting_correct == Inf) {
          reponse = "There are no Test Questions in this job. 
        Technically, workers can work as much as they want to. 
        We'd recommend setting a reasonable max work limit."
        } else  {
          response = paste("Max work limit is nto set. This is not good news.
          Set the limit to",max_setting_correct,"or lower.")
        }
      } else if (max_setting_correct == Inf) {
        reponse = "There are no Test Questions in this job. 
        Technically, workers can work as much as they want to. 
        We'd recommend setting a reasonable max work limit."
      } else if (max_setting_correct <= max_setting) {
        response = paste("The work limit is set too high! Set it to <b>", max_setting_correct, "</b>or lower immediately")
      } else if (max_setting_correct > max_setting) {
        response = paste("The work limit can be safely increased to <b>", 
                         max_setting_correct,
                         "</b>.")
      } else {
        response ="Something weird has happened. We've got nothing to say."
      }
      last_line = paste(rec_line,response)
      summary = paste(overview, comment, max_work_line, max_possible_line, last_line, sep="<br>")
      paste(summary)
    }
  })
  
  output$job_summary_message <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return("<p>No job data to return.</p>")
    } else {
      #Data to grab
      json = get_job_settings_from_json()
      workset = pull_workset_data()
      workset = as.data.frame(workset)
      
      
      #Variables to Display
      job_title = json$title 
      job_state = json$state
      support_email = json$support_email
      num_gold_units = json$golds_count
      num_nongold_units = json$units_count - json$golds_count
      
      total_judgments = sum(workset$judgments_count)
      
      untrusted_judgments = 
        sum(worksets$judgments_count[worksets$tainted == "t"])
      trusted_judgments = total_judgments - untrusted_judgments
      
      if (num_gold_units == 0) {
        gold_message = "<p style='color:red;'>It looks like there are NOT any <b>TQs</b> in this job. Unless this is a survey job or a doublepass job, they should use test questions."
      } else {
        gold_message = ""
      }
      
      overall_message = paste("<h5>Job Summary</h5>",
                              "<ul class=\"unstyled\"><li>Job Title:<br>", job_title, "</li><br>",
                              "<li>State:", job_state, "</li>",
                              "<li>Email:", support_email, "</li>",
                              "<li>Units:", num_nongold_units, "</li>",
                              "<li>Test Questions:", num_gold_units, "</li><br>",
                              "<li>Total Judgments:", total_judgments, "</li>",
                              "<li>Untrusted Judgments:", untrusted_judgments, "</li>",
                              "<li>Trusted Judgments:", trusted_judgments, "</li>",
                              "</ul>", sep="")
      
      paste(overall_message, gold_message)
    } 
  })
  
  
})
