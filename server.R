## Job Heart Monitor
## May 11, 2014
## 5 min job health

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('stringr')
require('reshape2')
require('stringr')
library('XML')
library('rjson')
require('rCharts')

source('get_job_data.R')
source('get_unit_data.R')
#source('get_judgment_data.R')
#source('get_country_data.R')
source('get_channel_data.R')
#source('get_worker_data.R')
source('db_call_function.R')
source('run_this_query_function.R')
source('work_available_query_function.R')
source('payrate_satisfaction_query_function.R')
source('dropout_rate_query_function.R')
source('find_cml_elements.R')
source('velocity_violations_query_function.R')
source('everyone_available_query_function.R')
source('worker_stats_query_function.R')


temp_dir = "/tmp/job_health"

system(paste('mkdir -p',temp_dir))

options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)
options(scipen=999)

auth_key = "5b7d73e5e7eb06556f12b45f87b013fc419f45f2"

db_call = db_call(connection_file = "builder_readonly")
#count html instructions length
instructions_length <- function(json_instructions){ 
  
  inst_array = unlist(lapply(strsplit(json_instructions, 
                                      split="\n")[[1]], function(x) strsplit(x, " ")))
  inst_clean = inst_array[str_detect(inst_array, pattern="\\w+")]
  word_count = length(inst_clean)
  return(word_count)
}

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
        db = db_call
        query = get_channel_data(job_id) 
        file = paste0(temp_dir,"/",
                      "builder_channels", "_", job_id, "_",
                      format(Sys.time(), "%b_%d_%X_%Y"),
                      ".csv")
        data = run_this_query(db, query, file)
        data
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
  
  pull_worker_stats <- reactive({
    if(input$get_job == 0 ||  input$job_id == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      print("in pull_worker_stats")
      db = db_call
      print("line 111")
      query = worker_stats_query(job_id)
      file = paste0(temp_dir,"/",
                    "worker_stats", "_", job_id, "_",
                    format(Sys.time(), "%b_%d_%X_%Y"),
                    ".csv")
      data = run_this_query(db, query, file)
      
      data
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
        db = db_call
        query = get_job_data(job_id) 
        file = paste0(temp_dir,"/",
                      "builder_jobs", "_", job_id, "_",
                      format(Sys.time(), "%b_%d_%X_%Y"),
                      ".csv")
        data = run_this_query(db, query, file)
        data
      }
    }
  })
  
  pull_unit_data <- reactive({
    if(input$get_job == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      if (job_id == 0) {
        return(NULL)
      }else{
       print("in pull_unit_data")
       db = db_call
       query = get_unit_data(job_id) 
       file = paste0(temp_dir,"/",
                     "builder_units", "_", job_id, "_",
                     format(Sys.time(), "%b_%d_%X_%Y"),
                     ".csv")
       data = run_this_query(db, query, file)
       data
        
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
        worker_stats = pull_worker_stats()
        data = worker_stats[,c("worker_id", "id", "golden_trust", "golds_count", 
                               "missed_count", "forgiven_count", "judgments_count", 
                               "flagged_at", "rejected_at", "tainted")]
        data
      } 
    }
  })
  
  output$worksetData <- renderTable({
    if(input$get_job == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      if (job_id == 0) {
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
        print(query)
        file = paste0(temp_dir,"/",
                      "work_available", "_",
                      format(Sys.time(), "%b_%d_%X_%Y"),
                      ".csv")
        data = run_this_query(db, query, file)
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
        data
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
  
  
  #   states = c("maxed out", "working",
  #              "tainted","checked_out", "not_in_yet")
  get_state_counts<- reactive({
    if (input$get_job == 0 || input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      maxed_out = as.numeric(get_num_maxed_out())
      if(is.na(maxed_out)){
        maxed_out = 0
      }
      print(310)
      working = nrow(pull_judgment_counts())
      if(is.na(working)){
        working = 0
      }
      print(312)
      tainted = sum(pull_tainted_breakdown_data()$y)
      if(is.na(tainted)){
        tainted = 0
      }
      print(314)
      checked_out = as.numeric(get_number_checked_out())
      if(is.na(checked_out)){
        checked_out = 0
      }
      print(316)
      all_available_workers = as.numeric(get_everyone_available())
      if(is.na(all_available_workers)){
        all_available_workers = 0
      }
      
      not_in_yet = all_available_workers - (maxed_out + working + tainted + checked_out)
      all_v = c(maxed_out, working, tainted, checked_out, not_in_yet)
      return(all_v)
    }
  })
  
  get_number_checked_out <- reactive({
    if (input$get_job == 0 || input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = input$job_id
      print("in get_number_checked_out")
      worker_stats = pull_worker_stats()
      data = sum(worker_stats$no_judgments==1)
      data
      
    }
  })
  
  
  output$throughput_bar <- renderChart({
    if (input$get_job == 0 || input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      print("in output$throughput_bar")
      job_id = input$job_id
      states = c("maxed out", "working",
                 "tainted","checked_out", "not_in_yet")
      div_names = c("maxed_out_div",
                    "working_div",
                    "tainted_div",
                    "checked_out_div",
                    "not_in_yet_div")
      print(335)
      state_counts = get_state_counts() 
      print(337)
      responses_table_transformed = data.frame(group=states,
                              y = state_counts, # these are the numbers found in groups
                              x=rep(as.character(job_id),times=5), # this is a fake grouping variable
                              preserve_order = 1:5,
                              info = c("<b>Maxed out</b>",
                                       "<b>Active workers who can still make more judgments</b>",
                                       "<b>Tainted workers</b>",
                                       "<b>Checked the job out, didn't make judgments</b>",
                                       "<b>Never entered the job</b>"), # this is a vector of html vars describing tooltips
                                        click_action = div_names
      )
      
      
      data_list = lapply(split(responses_table_transformed, responses_table_transformed$group),
                   function(x) {
                     res <- lapply(split(x, rownames(x)), as.list)
                     names(res) <- NULL
                     return(res)
                     })
      
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
      h1
      
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
      #print(job_json)
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
      max_work_setting = as.numeric(get_max_setting())
      workers_with_work = pull_judgment_counts()
      workers_who_maxed_out = workers_with_work[workers_with_work$judgments_count == max_work_setting,]
      num_maxed_out = nrow(workers_who_maxed_out)
      return(num_maxed_out)
    }
  })
  
  get_everyone_available <- reactive ({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = input$job_id
      print("in get_everyone_available")
      db = db_call
      json = get_job_settings_from_json()
      skill_vector = names(json$minimum_requirements$skill_scores)
      # need to check for reuire some / all here
      print(skill_vector)
      country_include_vector = unlist(
        lapply(json$included_countries, function(x) x$name)
      )
      print(country_include_vector)
      country_exclude_vector =  unlist(
        lapply(json$excluded_countries, function(x) x$name)
      )
      print(country_exclude_vector)
      min_required_skills = json$minimum_requirements$min_score
      query = everyone_available_query(skills = skill_vector,
                                       countries_include = country_include_vector,
                                       countries_exclude = country_exclude_vector,
                                       min_score = min_required_skills)
      file = paste0(temp_dir,"/",
                    "everyone_available", "_", job_id, "_",
                    format(Sys.time(), "%b_%d_%X_%Y"),
                    ".csv")
      data = run_this_query(db, query, file)
      #data = read.csv(paste0(temp_dir,"/",
      #                       "everyone_available_385528_May_23_15:38:15_2014.csv"))
      #data= data[1,]
      print("Workset server Line 518")
      print(names(data))
      print(head(data))
      data
    }
  })
  
  output$maxed_out_summary <- renderText({
    if (input$get_job == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      line1 = "<div class=\"bar_divs well\" id=\"maxed_out_div\" style=\"display: none;\">"
      title ="<h4>Maxed Out</h4>"
      num_maxed_out = get_num_maxed_out()
      max_setting = get_max_setting()
      max_setting_correct = get_max_setting_correct()
      overview = paste0(num_maxed_out," workers have maxed out")
      comment = "(This means that they cannot do any more work)"
      max_work_line = paste("Max work per worker is set to:",max_setting)
      max_possible_line = paste("This job's params would allow for a work limit of:",max_setting_correct)
      rec_line = "The gist is:<br>"
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
      closing_div = "</div>"
      summary = paste(line1, title, overview, comment, max_work_line, 
                      max_possible_line, last_line, closing_div, sep="<br>")
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
    #workset = pull_workset_data()
    #workset = as.data.frame(workset)
      
   units = pull_unit_data()
      
    #Variables to Display
   job_title = json$title 
   job_state = json$state
    #support_email = json$support_email
    #num_gold_units = json$golds_count
    #num_nongold_units = json$units_count - json$golds_count
   inprogress_new_units = units[units$state < 6,]
      
   enabled_golds = units[units$state == 6,]
   disabled_golds = units[units$state == 7,]
      
   canceled_units = units[units$state == 8,]
   finalized_units = units[units$state == 9,]
      
   num_gold_units = nrow(enabled_golds)
   num_disabled_golds = nrow(disabled_golds)
   num_finalized_units = nrow(finalized_units)
   num_new_units = nrow(inprogress_new_units)
   num_canceled_units = nrow(canceled_units)
      
    #total_judgments = sum(workset$judgments_count - workset$golds_count)      
    #tainted_work = 
    #  workset[workset$tainted == "t",]
      
    #untrusted_all = sum(tainted_work$judgments_count)
    #untrusted_golds = sum(tainted_work$golds_count)
      
    #untrusted_judgments = untrusted_all - untrusted_golds
    #trusted_judgments = total_judgments - untrusted_judgments
           
   if(num_canceled_units > 0){
     canceled_message = paste("<li>Canceled Units: ", num_canceled_units, "</li>")
   } else {
     canceled_message = ""
   }
      
   if(num_disabled_golds > 0){
     tq_message = paste("<li> Enabled TQs: ", num_gold_units, "</li>",
        "<li> Disabled TQs:", num_disabled_golds, "</li>")
   } else {
     tq_message = paste("<li> Test Questions: ", num_gold_units, "</li>")
   }
      
   overall_message = paste("<h5>Job Summary</h5>",
                            "<ul class=\"unstyled\"><li>Job Title:<br>", job_title, "</li><br>",
                            "<li>State: ", job_state, "</li>",
                            "<li>Finalized Units: ", num_finalized_units, "</li>",
                            "<li>In Progress &amp; New Units: ", num_new_units, "</li>",
                            canceled_message, tq_message,
                            "</ul>", sep="")
      
    paste(overall_message)
  } 
 })
  
  output$job_settings_message <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return("<p>No job data to return.</p>")
    } else {
      #Data to grab
      json = get_job_settings_from_json()
      channels = pull_channel_data()
      
      #Max Work Settings
      mjw = json$max_judgments_per_worker
      mjip = json$max_judgments_per_ip
      
      #Skills Required
      skills <- json$minimum_requirements$skill_scores
      skill_names = names(skills)
      
      #QM Settings
      quiz_mode = json$options$front_load
      if(is.null(quiz_mode)){
        quiz_mode = "FALSE"
      }
      after_gold = json$options$after_gold
      
      #Units and Payments
      upa = json$units_per_assignment
      ppt = json$payment_cents
      
      #Number of Channels
      num_channels = length(channels$channel_name)

      overall_message = paste("<h4>Settings Summary</h4>",
                        "<p>Max Work per Contributor: ", mjw, "<br>",
                        "Max Work per IP: ", mjip, "<br>",
                        "Skill Requirements: ", paste(skill_names, collapse="\n"), "<br>",
                        "Quiz Mode: ", quiz_mode,"<br>",
                        "Units per Task: ", upa,"<br>",
                        "Payment per Task: ", ppt, "<br>",
                        "Number of Channels Enabled: ", num_channels, "<br>",
                        "</p>", sep="")

      paste(overall_message)
    } 
  })
  
  output$job_settings_warnings <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return("")
    } else {
      #Data to grab
      json = get_job_settings_from_json()
      
      #Max Work Settings
      mjw = json$max_judgments_per_worker
      mjip = json$max_judgments_per_ip
      
      #Levelled Crowd
      skills <- json$minimum_requirements$skill_scores
      skill_names = names(skills)
      
      #Quiz Mode Settings
      quiz_mode = json$options$front_load
      
      count = length(skill_names[grepl("level_\\d_contributors", skill_names)])
      
      if (is.null(mjw)) {
        mjw_message = "<p><i class=\"icon-resize-small\"></i> Whoa. The max work per judgments setting is empty. 
        This means contributors can be in the job for as long as they want 
        and have the opportunity to learn TQ's. 
        See the success center docs for more info on how to set this.</p>"
      } else {
        mjw_message = ""
      }
      
      if (is.null(mjip)) {
        mjip_message = "<p><i class=\"icon-resize-small\"></i><i class=\"icon-warning-sign\"></i> Ah oh. There is no Max Work per IP set.
        This means someone coming from one IP can contribute work with many contributor IDs.
        See the success center docs for more info on how to set this.</p>"
      } else {
        mjip_message = ""
      }
      
      if(count == 0){
        skill_message = "<p><i class=\"icon-filter\"></i> Hmmm. We did not detect a leveled crowd. 
                        We highly recommend you use a levelled crowd for all jobs.</p>"
      } else {
        skill_message = ""
      }
      
      if(is.null(quiz_mode)){
        qm_message = "<p><i class=\"icon-pencil\"></i> So there is no quiz mode in this job. 
        Is that on purpose?</p>"
      } else {
        qm_message=""
      }
      
      if(mjw_message == "" && mjip_message == "" && skill_message == "" && qm_message == ""){
        paste("<p class=\"alert alert-success\">
              <i class=\"icon-ok\"></i>
              <big>Job Settings Errors:</big>
              <br>We did not detect any obvious errors.</p>")
      } else{
        paste("<div class=\"alert alert-error\">", "<p><big>Job Settings Errors:</big></p>", 
              mjw_message, mjip_message, skill_message, qm_message, "</div>")
      }
    }
  })
  
  output$job_settings_cautions <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      return("<p>Waiting to pull job json.</p>")
    } else {
      
      #Data to Grab
      json = get_job_settings_from_json()
      #gold answers - grab values per cml_name
      
      #Grab Elements
      cml = json$cml
      instructions = json$instructions
      actual_pay = json$payment_cents
      upa = json$units_per_assignment
      
      #instructions length
      length_inst = instructions_length(instructions)
      
      if(length_inst > 1000){
        inst_message = paste("<p><u>TLDR Warning</u>: The instructions seem pretty long. 
                       Make sure this job cannot be broken down into simplier parts. 
                       Or add essential tips and tricks into the task itself.</p>",
                      "<p><b>Instructions Word Count</b>:", length_inst,"</p>")
      } else if(length_inst < 100){
        inst_message = paste("<p><u>Edge Case Warning</u>: These instructions are super short. 
                       Are you sure you covered all your cases in the examples section of the instructions? 
                       You can ignore this warning if the task is a survey.</p>",
                       "<p><b>Instructions Word Count</b>:", length_inst,"</p>")
      } else{
        inst_message = ""
      }
      
      #html errors
      
      #Bad Pricing - based on calculator
      find_texts = '<cml:text(area)?(\\s|\\w|=|:|\"|\\[|\'|\\,|\\]|\\{|\\})*(required)'
      find_clicks = 'href=\\"https?://'
      #search number is a subset of clicks
      find_search = 'href=\\"https?://(\\w|\\.|/)+(search)'
      
      click_markup = 0
      search_markup = 0
      text_markup = 0
      level_markup = 0
      
      if(str_detect(cml, pattern=find_search)){
        search_markup = .7
      }
      
      if(str_detect(cml, pattern=find_clicks) && !(str_detect(cml, pattern=find_search))){
        click_markup = .2	
      }
      
      if(str_detect(cml, pattern=find_texts)){
        text_markup = .3
      } 
      
      ##Level Mark Ups
      if(!(is.null(json$minimum_requirements$skill_scores$level_3_contributors))){
        level_markup = .3
      }
      
      if(!(is.null(json$minimum_requirements$skill_scores$level_2_contributors))){
        level_markup = .2
      }
      
      pay_per_unit = 1 + click_markup + search_markup + text_markup + level_markup
      suggested_pay = upa * pay_per_unit
      
      error = abs((suggested_pay - actual_pay)/suggested_pay)
      direction = suggested_pay - actual_pay
      if(error > .25){
        if(direction < 0){
          pay_warning = paste("<p><u>Over Payment Caution</u>: We believe you might be overpaying for this type of task. 
          If you are getting the results you want within your expected cost then you may ignore this message. 
          If not we suggest you pause the job and recalibrate the payments.</p>", "<p><b>Current Pay per Task:</b>",
            actual_pay, "<br><b>Suggested Pay per Task:</b>", suggested_pay, "</p>")
        }
        if(direction > 0){
          pay_warning = paste("<p><u>Under Payment Warning</u>: We believe based on the requirements of this task and 
          the size of a single task that you are under paying. Please consider increasing the contributor pay or 
          decreasing the assignment size.</p>", "<p><b>Current Pay per Task:</b>", actual_pay, "<br><b>Suggested Pay per Task:</b>", 
          suggested_pay, "</p>")
        }
      } else {
        pay_warning = ""
      }
      
      
      #Units per Assignment - based on calculator
      #Count validated fields number of fields
      find_validates = "validates=\"(\\w|:|\\[|\\]|\\'|\\,|\\{|\\})*\\s?(required)"
      count_validates = str_count(cml, pattern=find_validates)
      vpa = upa * count_validates
      if(vpa > 19){
        unit_warning = paste("<p><u>Long Task Warning</u>: Careful, we found an exorbitant amount of required fields in one task. 
                             You may want to decrease the Units per Task. 
                             If this is an image moderations type task you may ignore this message.</p>", 
                             "<p><b>Number of Required Fields per Task:</b>", vpa,"</p>")
      } else if(vpa < 6){
        unit_warning = paste("<p><u>Short Task Warning</u>: Careful, it seems like there are not many required fields per task. 
                             You may want to up the Units per Task setting to get more bang for your buck.</p>", 
                             "<p><b>Number of Required Fields per Task:</b>", vpa,"</p>")
      } else {
        unit_warning =""
      }
      
      if(pay_warning == "" && inst_message == "" && unit_warning == ""){
        paste("<div class=\"alert alert-success\"><p><i class=\"icon-ok\"></i>
              <big>Job Settings &amp; Design Concerns:</big>
              <br>We do not have any suggestions on approving job design. 
              Make sure to check the Job Settings Error section above before moving on.</p>",
              "<ul class=\"unstyled\"><li>Suggested Pay: ", suggested_pay, " Actual Pay: ",
              actual_pay,"</li><li> Instructions Word Count: ", length_inst, 
              "</li><li>Number of Required Fields per Task: ", vpa, "</li></ul></div>")
      }else{
        paste("<div class=\"alert\">", "<p><big>Job Settings &amp; Design Concerns:</big></p>", 
              pay_warning, inst_message, unit_warning, "</div>")
      }
    }
  })
  
  
  output$job_settings_overview <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      # User has not uploaded a file yet
      return("<p>No job data to return.</p>")
    } else {
      #Data to grab
      json = get_job_settings_from_json()
      job = pull_job_data()
    
      #Max Work Settings
      mjw = json$max_judgments_per_worker
      mjip = json$max_judgments_per_ip
      
      #Behavior settings
      #contributor limiting
      track_aliases = json$options$track_clones
      require_login = json$require_worker_login
      min_account_age = json$minimum_account_age_seconds
      
      min_account_age = min_account_age/(24*3600)
      
      #rate limiting
      ip_speed = job$max_assignments_per_minute
      ip_flag = json$options$flag_on_rate_limit
      
      id_speed = job$min_assignment_duration 
      id_flag = job$flag_on_min_assignment_duration 
      
      if(id_flag == "f"){
        id_flag = "FALSE"
      }else{
        id_flag = "TRUE"
      } 
      
      id_email = job$send_emails_on_rate_limit
      if(id_email == "t"){
        id_email = "TRUE"
      } else {
        id_email = "FALSE"
      }
    
      #Skills Required
      skills <- json$minimum_requirements$skill_scores
      skill_names = names(skills)
    
      count = length(skill_names[grepl("level_\\d_contributors", skill_names)])
  
      #QM Settings
      quiz_mode = json$options$front_load
      after_gold = json$options$after_gold
      min_accuracy = json$options$reject_at
      gold_per_task = json$gold_per_assignment
      
      #Task & Judgments Settings
      upa = json$units_per_assignment
      pay = json$payment_cents
      pages = json$pages_per_assignment
      task_exp = json$options$req_ttl_in_seconds
      task_exp = as.double(task_exp)/60
      
      jpu = json$judgments_per_unit
      remain_finalized = json$units_remain_finalized
      variable_mode = json$variable_judgments_mode
      
      max_judgments = json$max_judgments_per_unit
      expected_judgments = json$expected_judgments_per_unit
      stop_above_conf = json$min_unit_confidence
      confidence_fields = paste(json$confidence_fields, collapse=", ")
      
      if(variable_mode == "none" || variable_mode == "external"){
        variable_settings = paste("<li>Variable Judgments Mode: ", variable_mode,"</li>")
      } else {
        variable_settings = paste("<li>Variable Judgments Mode: ", variable_mode,"</li>",
                                  "<li>Max Judgments per Unit: ", max_judgments, "</li>",
                                  "<li>Expected Judgments: ", expected_judgments, "</li>",
                                  "<li>Stop Above Confidence: ", stop_above_conf, "</li>",
                                  "<li>Confidence Fields: ", confidence_fields, "</li>")
      }
      
      gold_task_settings = paste("<div class=\"row-fluid\">",
                                 "<span class=\"span5 left\"><h4>TQ Settings:</h4>", 
                                 "<ul class=\"unstyled\"><li>Quiz Mode: ", quiz_mode, 
                                 "</li><li>Minimum TQs in Job: ", after_gold,
                                 "</li><li>Minimum Accuracy: ", min_accuracy,
                                 "</li><li>TQs per Task: ", gold_per_task,
                                 "</li></ul></span>", 
                                 "<span class=\"span5 right\"><h4>Task &amp; Judgment Settings:</h4>",
                                 "<ul class=\"unstyled\"><li>Units per Task: ", upa,
                                 "</li><li>Payment per Task (cents): ", pay, "</li><li>Pages per Task: ",
                                 pages, "</li><li>Task Expiration (mins): ", task_exp, "</li><hr><li>",
                                 "Judgments per Unit: ", jpu, "</li><li>Units Remain Finalized: ",
                                 remain_finalized, "</li>", variable_settings, "</ul></span></div>")
      
      behavior_settings = paste("<div class=\"row-fluid\"><h4>Behaviour Settings:</h4>",
                                "<span class=\"span5 left\">",
                                "<ul class=\"unstyled\"><li>Max Judg per Contrib: ", mjw,
                                "</li><li>Max Judg per IP:", mjip,
                                "</li><li>Min Account Contr Age (days): ", min_account_age,
                                "</li><li>Max Task per Min: ", ip_speed,
                                "</li><ul class=\"unstyled\">",
                                "<li>Remove Contrib when Triggered: ", ip_flag,
                                "</li></ul><li>Require Login: ",require_login,
                                "</li></ul></span><span class=\"span5 right\">",
                                "<ul class=\"unstyled\"><li>Min Seconds per Task: ",
                                id_speed, "</li><ul class=\"unstyled\">",
                                "<li>Remove Contrib when Triggered: ",
                                id_flag, "</li><li>Email Owner when Triggered: ",
                                id_email, "</li></ul><hr><li>Track Aliases: ", track_aliases,
                                "</li></ul></span></div>")
    
     paste(gold_task_settings,"<hr>", behavior_settings)
    } 
  })
  
  output$throughput_errors <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
      return("<p>Awaiting Data.</p>")
    } else {
    workers = get_state_counts()
    #all_v = c(maxed_out, working, tainted, checked_out, not_in_yet)
    available = as.numeric(get_everyone_available())
    
    maxed =  workers[1]
    viable = workers[2]
    tainted = workers[3]
    check_out = workers[4]
    dont_care = workers[5]
    
    print("Number of workers available")
    print(available)
    #reject_at = json$options$reject_at
    if(available < 100 || is.na(available) || is.null(available)){
      too_small = "<p><i class=\"icon-minus-sign\"></i> <b>Hold Up: The contributor pool for this job is very small. 
      You need to consider broadening it (targetting more countries, levels, etc) 
      or resetting your throughput expectations.</b></p>"
    } else {
      too_small=""
    }
    
    total_worked = viable + maxed + tainted + check_out
    total = total_worked + dont_care
    
    percent_viable = (viable/total_worked)*100
    percent_maxed = (maxed/total_worked) * 100
    percent_tainted = (tainted/total_worked) * 100
    percent_check_out = (check_out/total_worked) * 100
    percent_dont_care = (dont_care/total) * 100
    
    if(is.nan(percent_viable) || is.infinite(percent_viable)){
      percent_viable = 0
    }
    
    if(is.nan(percent_maxed) || is.infinite(percent_maxed)){
      percent_maxed = 0
    }
    
    if(is.nan(percent_tainted) || is.infinite(percent_tainted)){
      percent_tainted = 0
    }
    
    if(is.nan(percent_check_out) || is.infinite(percent_check_out)){
      percent_check_out = 0
    }
    
    if(is.nan(percent_dont_care) || is.infinite(percent_dont_care)){
      percent_dont_care = 0
    }
    
    if(percent_tainted > 35){
      failure_message = "<p><i class=\"icon-remove-sign\"></i> Ah oh: We're getting a lot of failures in
      work mode. You may want to check on the Test Questions and the reject_at rate.</p>"
    } else {
      failure_message = ""
    }
    
    if(percent_maxed > 50){
      maxed_message = "<p><i class=\"icon-resize-full\"></i> Note: Over %50 of the workers in the job have maxed out. If the job is not near to finishing you may want to add more TQs or up the max work settings.</p>"
    } else {
      maxed_message = ""
    }
    
    if(too_small == "" && failure_message == "" && maxed_message == ""){
      paste("<p class=\"alert alert-success\">
             <i class=\"icon-ok\"></i>
             <big>Throughput Contributor Concerns:</big>
             <br>We did not detect any obvious concerns/issues.</p>")
    } else {
      paste("<div class=\"alert alert-error\">", "<p><big>Throughput Contributor Errors:</big></p>",
            too_small, failure_message, maxed_message, "</div>")
    }
   } 
  })
  
  output$throughput_warnings <- renderText({
    if (input$get_job == 0 || input$job_id == 0) {
    # User has not uploaded a file yet
      return("<p>Waiting for Data.</p>")
    } else {
    workers = get_state_counts()
    #all_v = c(maxed_out, working, tainted, checked_out, not_in_yet)
    
    #available = as.numeric(get_everyone_available)
    
    maxed =  workers[1]
    viable = workers[2]
    tainted = workers[3]
    check_out = workers[4]
    dont_care = workers[5]
    
    #quiz mode failures have not been pulled
    #qm_fail = 15
    total_worked = viable + maxed + tainted + check_out
    total = total_worked + dont_care
    percent_viable = (viable/total_worked)*100
    percent_check_out = (check_out/total_worked) * 100
    percent_dont_care = (dont_care/total) * 100
    
    if(is.nan(percent_viable) || is.infinite(percent_viable)){
      percent_viable = 0
    }
    
    if(is.nan(percent_check_out) || is.infinite(percent_check_out)){
      percent_check_out = 0
    }
    
    
    if(percent_viable < 20){
      viable_message = "<p>Careful: Looks like your group of active contributors is dwindling.</p>"
    } else {
      viable_message = ""
    }
    
    if(percent_check_out > 35){
      lookers_message = "<p>Ah oh: We're seeing a high percentage of contributors just looking at the task or giving up after quiz mode. You may want to up the Payment per Task or broaden your contributor target.</p>"
    } else {
      lookers_message = ""
    }
    
    if(percent_dont_care > 50){
      dont_care_message ="<p>Yikes: Over 50% of people eligible for this job have not even looked at it. You
       may want to increase the pay or make the job a little easier.</p>"
    } else {
      dont_care_message = ""
    }
    
    if(viable_message == "" && lookers_message == "" && dont_care_message == ""){
      paste("<p class=\"alert alert-success\">
             <i class=\"icon-ok\"></i>
             <big>Throughput Contributor Cautions:</big>
             <br>We do not have any suggestions at this time. Make sure to review any issues in the Throughput
             Contributor Errors section above.</p>")
    } else {
      paste("<div class=\"alert\">", "<p><big>Throughput Contributor Cautions:</big></p>",
            viable_message, lookers_message, dont_care_message, "</div>")
    }
  }
 })
  
 ###Quality Warnings
 output$quality_gold_errors <- renderText({
  if (input$get_job == 0 || input$job_id == 0) {
     return("<p>Waiting to pull builder_units.</p>")
  } else {
    #Data to Grab
    #json = get_job_settings_from_json()
    units = pull_unit_data()
    #gold answers - grab values per cml_name
    print("Did you make it here? 1186")
    enabled_golds = units[units$state == 6,]
    print("ENABLED GOLDS")
    print(enabled_golds)
    
    disabled_golds = units[units$state == 7,]
    print("DISABLED GOLDS")
    print(disabled_golds)
     
    num_units = nrow(units)
    num_golds = nrow(enabled_golds)
    
    if(num_golds != 0){
      enabled_golds$missed_percent = enabled_golds$missed_count/enabled_golds$judgments_count
      enabled_golds$contested_percent = enabled_golds$contested_count/enabled_golds$missed_count
      
      highly_missed = enabled_golds[enabled_golds$missed_percent > .67,]
      highly_contested = enabled_golds[enabled_golds$contested_percent > .50,]
      
      num_missed = nrow(enabled_golds[enabled_golds$missed_count > 0,])
      
      #More than 9% of golds are missed +67% of the time
      if(nrow(highly_missed)/num_golds > .09){
        tq_missed_message = "<p><i class=\"icon-edit\"></i> Missed TQ's: There are quite a few test questions that are highly missed. 
        We would update those before digging into Quality too much.</p>"
      } else {
        tq_missed_message = ""
      }
     
      #More than 19% of golds are contested +50% of the time
      if(nrow(highly_contested)/num_missed > .19){
        tq_contested_message = "<p><i class=\"icon-edit\"></i> Contested TQ's: There are quite a few missed test questions that are highly contested.</p>"
      } else{
        tq_contested_message = "" 
      }
 
      #Enough Golds
      #wrt number of units for every 100 units there should be AT LEAST 10 units.
      if(num_golds/num_units < .11){
        enough_golds_message = "<p><i class=\"icon-list-alt\"></i> Careful: There are very few golds given the number of units. 
        You may want to increase it.</p>"
      } else {
        enough_golds_message = ""
      }
 
      #Bad Validators
      #If a gold answer does not match the validators (for text) or names/values (for non text)
      #if(i < j){
      #  validators_message = "<p><i class=\"icon-fire\"></i><b> WARNING! We detected that some of the answers provided in TQs DO NOT match the values provided in CML. 
      #  Please pause and fix this before continuing.</b><p>"
      #} else {
      #  validators_message = ""
      #}
    
      if(tq_missed_message == "" && enough_golds_message == "" && tq_contested_message == ""){
        paste("<p class=\"alert alert-success\">
             <i class=\"icon-ok\"></i>
             <big>Gold Quality Warnings:</big>
             <br>We did not detect any obvious errors.</p>")
      } else {
        paste("<div class=\"alert alert-error\">", "<p><big>Gold Quality Warnings:</big></p>",
        tq_missed_message, tq_contested_message, enough_golds_message, "</div>")
      }  
    } else {
      paste("<h4 class=\"alert alert-error\"> We've detected no enabled Test Questions in this task. Unless this
            is a survey or a content creation job we highly suggest you use Test Questions.</h4>")
    }
  }
})
  
  output$quality_times_warnings <- renderText({
    #if (input$get_job == 0 || input$job_id == 0) {
    # User has not uploaded a file yet
    #  return("<p>No job data to return.</p>")
    #} else {
    
    #Data to Grab
    #grab scambot data
      i = 4
      j = 5
    
      if(i < j){
        times_message ="<p><i class=\"icon-time\"></i> Caution: We've detected some speed demons in this task. 
        You may want to take a look at them and update Speed Limit Settings when needed.</p>"
      } else{
        times_message =""
      }
      
      if(times_message == ""){
        paste("<p class=\"alert alert-success\">
              <i class=\"icon-ok\"></i>
              <big>Completion Times Warnings:</big>
              <br>We did not detect any speed demons in this task.</p>")
        
      } else{
        paste("<div class=\"alert alert-error\"><p><big>Completion Times Warnings:</big></p>",
              times_message, "</div>")
      }
    #}
  })
  
  output$quality_cautions <- renderText({
    #if (input$get_job == 0 || input$job_id == 0) {
    # User has not uploaded a file yet
    #  return("<p>No job data to return.</p>")
    #} else {
    #Data to Grab
    #grab scambot data
    
    i = 4
    j = 5
    
    #Just Paste Contentions
    if(i < j){
      contentions = "<p> FYI: Here are some of the popular contentions 
      we are seeing in this job.</p>"
    } else {
      contentions = ""
    }
    
    #Diverse Golds
    #look up given answers wrt possible values in JSON
    #if unique gold answers < .75(unique values)
    if(i < j){
      diverse_message = "<p>It seems that your provided TQ answers do not encompass all of the provided values on some questions. 
      This might lead to misunderstandings; make sure to address any edge cases within instructions 
      and task design if they are not throughly explained through TQs.</p>"
    } else {
      diverse_message = ""
    }
    
    if(diverse_message == "" && contentions == ""){
      paste("<p class=\"alert alert-success\">
              <i class=\"icon-ok\"></i>
              <big>Quality Cautions:</big>
              <br>Your job is legit. We do not have any suggestions about Golds/Quality settings.</p>")
    } else {
      paste("<div class=\"alert\"><p><big>Quality Cautions:</big></p>",
            contentions, diverse_message, "</div>")
    }
    #}
  })

output$not_in_yet_summary <- renderText({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      line1 = "<div class=\"bar_divs well\" id=\"not_in_yet_div\" style=\"display: none;\">"
      title ="<h4>Not in yet</h4>"
      job_json = get_job_settings_from_json()
      price_per_task = as.numeric(job_json$payment_cents)
      upa = as.numeric(job_json$units_per_assignment)
      price_per_unit = price_per_task/upa
      ##### the graph
      h1 = price_available_plot()
      # you need to insert this as html/script, so the chart needs to be processed properly first
      price_chart_html = paste(capture.output(h1$show('inline')), collapse="")
      #####
      overview = paste0("Your job is currently paying ", price_per_task, 
                        " cents for a task of ", upa, "units",
                        " <b>(",price_per_unit," cents per unit)</b>")
      comment = "<br>See if the pay is relatively high or low compared to active jobs in the last 2 hours:"
      recommendation = "If your job's pay is low compared to others, contributors may not find it very attractive. Consider raising your pay rate."
      TODO = "Put current pay on the graph. Better histogramming solution. 
      Charts get stuck when I turn off categories."
      closing_div = "</div>"
      price_piece = paste(overview, comment, price_chart_html, recommendation, sep="<br>")
      ###### the price piece ends
      ## complexity will potentially be here too
      summary = paste(line1, title, price_piece,
                      closing_div, sep="<br>")
      paste(summary)
    }
  })

get_cml_fields <- reactive({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      # get job cml
      job_json = get_job_settings_from_json()
      cml = job_json$cml
      # find cml:text and cml:textareas
      
      # we can add stuff here like parsing labels and seeign if stuff is required
    }
  })

output$checked_out_summary <- renderText({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      line1 = "<div class=\"bar_divs well\" id=\"checked_out_div\" style=\"display: none;\">"
      title ="<h4>Checked the job out, made 0 judgments</h4>"
      job_json = get_job_settings_from_json()
      fields_in_job = job_json$confidence_fields
      num_fields_in_job = length(fields_in_job)
      cml = job_json$cml
      text_fields = find_cml_elements(what_to_find=c("text", "textarea"),
                                      where_to_look=cml) #get_cml_fields()
      #####
      overview = paste0("Your job has ", num_fields_in_job, 
                        " questions in it. <br>", text_fields, " are text fields.")
      comment = "Is this a little? Is this a lot? We don't know yet, but we'll learn soon."
      closing_div = "</div>"
      ## complexity will potentially be here too
      summary = paste(line1, title, overview,comment,
                      closing_div, sep="<br>")
      paste(summary)
    }
  })

price_available_plot <-  reactive({
    if(input$get_job == 0 || input$job_id == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      price_df = pull_work_available()
      if (nrow(price_df) == 0) {
        for_plot1 = data.frame(active_workers = 1,
                               cent_groups = 1,
                               skills = 1)
      } else {
        for_plot = aggregate(data = price_df, active_workers ~ cents_per_unit + skills, FUN=sum)
        # find last 10 percent of 
        top_10 = quantile(for_plot$cents_per_unit, 0.9)
        for_plot_sub = for_plot[for_plot$cents_per_unit < top_10,]
        for_plot_sub$cent_groups =  as.numeric( 
          sub("[^,]*,([^]]*)\\]", "\\1", 
              as.character(cut(for_plot_sub$cents_per_unit, breaks=20))
          )
        )
        for_plot1 = aggregate(data=for_plot_sub, active_workers ~ cent_groups + skills, FUN=sum)
      }
      
      h1 <- hPlot(active_workers ~ cent_groups,
                  data = for_plot1, 
                  type = c("column"), 
                  group = "skills")
      max_mark = aggregate(data=for_plot1, active_workers ~ cent_groups, FUN=sum)
      h1$plotOptions(
        series = list(
          stacking = "normal",
          pointWidth = 10,
          type= "category")
      )
    } 
    
    ######################## experimental section ###########################
    #series1 = everything with prices
    agg1 = aggregate(data = price_df, active_workers ~ cents_per_unit + skills, FUN=sum)
    series1 = split(x = agg1, f = rownames(agg1))
    #series2 = only more / less
    this_price = 2.5
    more_less = price_df
    more_less$less_than_yours = price_df$cents_per_unit <= 2.5
    agg2 = aggregate(data = more_less, active_workers ~ cents_per_unit + less_than_yours + skills, FUN=sum)
    series2 = split(x = agg2, f = agg2$less_than_yours)
    df1 = split(series2[[1]], 1:nrow(series2[[1]]))
    df2 = split(series2[[2]], 1:nrow(series2[[2]]))
    #make plot
    h2 <- rCharts::Highcharts$new()
    #h2$series(data = series1, type = "column", id = "prices") # add the detailed series
    main_df = list(
      list(name = "less than you", drilldown = "less_than_yours", y = sum(
        more_less$active_workers[more_less$less_than_yours == T]
      )
      ),
      list(name = "more than you", drilldown = "more than yours", y = sum(
        more_less$active_workers[more_less$less_than_yours == F]
      )
      )
    )
    h2$series(data = main_df, name = 'Things')
    #h2$drilldown$series(data = df1, type = "column", id = "less_than_yours") # add the more less series
    #h2$drilldown$series(data = df2, type = "column", id = "more than yours") # add the more less series
    #########################################################################
    
    h1
    
  })

tainted_bar <- reactive({
    if (input$get_job == 0 || input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = input$job_id
      print(673)
      tainted_breakdown_data = pull_tainted_breakdown_data()
      print(675)
      data_list = lapply(split(tainted_breakdown_data, tainted_breakdown_data$group),
                         function(x) {
                           res <- lapply(split(x, rownames(x)), as.list)
                           names(res) <- NULL
                           return(res)
                         }
      )
      print(681)
      h1 <- rCharts::Highcharts$new()
      invisible(sapply(data_list, function(x) {
        h1$series(data = x, type = "column", name = x[[1]]$group)
      }
      ))
      h1$tooltip(useHTML = T, formatter = 
                   "#! function() { return('<b>' + this.point.group + '</b><br>' + 'Num workers: ' + this.point.y); } !#")
      h1$addParams(dom = 'tainted_bar')
      print(688)
      print(h1)
    }
  })
  
  pull_tainted_breakdown_data <- reactive({
    if(input$get_job == 0 || input$job_id == 0){
      return(NULL)
    } else {
      job_id = input$job_id
      speed_violations = pull_speed_violations()
      answer_distributions = pull_answer_flags()
      print(699)
      trust_eliminations = pull_trust_taints()
      all_tainted = pull_everyone_tainted()
      print(702)
      # ideally we would merge these real nice to remove duplicates
      # TODO we should check between these dbs for duplicates
      speed_limit_count = length(unique(speed_violations$worker_id))
      answer_distributions_count = length(unique(answer_distributions$worker_id))
      trust_eliminations_count = length(unique(trust_eliminations$worker_id))
      tained_for_other_reasons = all_tainted$worker_id[!(all_tainted$worker_id 
                                                         %in% c(speed_violations$worker_id,
                                                                answer_distributions$worker_id,
                                                                trust_eliminations$worker_id))]
      tained_for_other_reasons_count = length(unique(tained_for_other_reasons))
      group_categories= c("Speed limit violations", "Answer distribution violations",
                          "Low trust (tq)","Other reasons")
      print(715)
      tainted_breakdown_data = data.frame(group=group_categories,
                                          y = c(speed_limit_count,
                                                answer_distributions_count,
                                                trust_eliminations_count,
                                                tained_for_other_reasons_count), # these are the numbers found in groups
                                          x=rep("", times=4), # this is a fake grouping variable
                                          preserve_order = c(5,4,3,2)
      )
      print(724)
      return(tainted_breakdown_data)
    }
    
  })

pull_speed_violations <- reactive({
    if(input$get_job == 0 || input$job_id == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      print("in pull_speed_violations")
      db = db_call
      query = velocity_violations_query(job_id)
      file = paste0(temp_dir,"/",
                    "speed_violations", "_", job_id, "_",
                    format(Sys.time(), "%b_%d_%X_%Y"),
                    ".csv")
      data = run_this_query(db, query, file)
      print("Workset server Line 772")
      print(names(data))
      print(head(data))
      data
    } 
  })
  
  pull_answer_flags <- reactive({
    if(input$get_job == 0 || input$job_id == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      print(job_id)
      worker_stats = pull_worker_stats()
      data = worker_stats[worker_stats$answer_distribution_flags == 1,c("worker_id", "flag_reason")]
      data
    } 
  })
  
  pull_trust_taints <- reactive({
    if(input$get_job == 0 || input$job_id == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      print("in pull_trust_taints")
      print(job_id)
      worker_stats = pull_worker_stats()
      print("line 825")
      print(head(worker_stats))
      data = worker_stats[worker_stats$trust_taint == 1, c("worker_id", "golden_trust")]
      print("and now data")
      print(head(data))
      data
    } 
  })
  
  pull_everyone_tainted <- reactive({
    if(input$get_job == 0 || input$job_id == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      worker_stats = pull_worker_stats()
      data = worker_stats[worker_stats$tainted == 't' | worker_stats$tainted == 'true',
                          c("worker_id", "tainted", "flagged_at", "rejected_at", 
                            "flag_reason", "golden_trust")]
      data
    } 
  })
  
  output$tainted_summary <- renderText({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      line1 = "<div class=\"bar_divs well\" id=\"tainted_div\" style=\"display: none;\">"
      title ="<h4>Tainted workers</h4>"
      print(821)
      tainted_breakdown = pull_tainted_breakdown_data()
      print(823)
      all_tainted = sum(tainted_breakdown$y)
      print(825)
      ##### the graph
      h1 = tainted_bar()
      # you need to insert this as html/script, so the chart needs to be processed properly first
      chart_html = paste(capture.output(h1$show('inline')), collapse="")
      #####
      overview = paste0("There are ", all_tainted, 
                        " tainted workers in your job.")
      comment = "Check out the breakdown below to see the main reason(s) why they are tainted."
      recommendation = ""
      closing_div = "</div>"
      ###### the price piece ends
      ## complexity will potentially be here too
      summary = paste(line1, title, overview, comment, chart_html,
                      closing_div, sep="<br>")
      paste(summary)
    }
  })
  
  pull_judgment_counts <- reactive({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = input$job_id
      print("in pull_judgment_counts")
      worker_stats = pull_worker_stats()
      data = worker_stats[worker_stats$num_judgments > 0,c("worker_id", "num_judgments")]
      names(data) = c("worker_id", "judgments_count")
      if (nrow(data) == 0) {
        data[1,] = rep(0, times=ncol(data))
      } 
      data
    }
  })
  
  num_judgments_bars <- reactive({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers_with_judgments = pull_judgment_counts()
      job_id = input$job_id
      print(head(workers_with_judgments))
      workers_with_judgments = workers_with_judgments[order(workers_with_judgments$judgments_count),]
      workers_with_judgments$index = 1:nrow(workers_with_judgments)
      #https://crowdflower.com/jobs/443343/contributors/1863365
      workers_with_judgments$click_action = paste0("https://crowdflower.com/jobs/",
                                                   job_id, 
                                                   "/contributors/",
                                                   workers_with_judgments$worker_id)
      
      
      workers_with_judgments$worker_id = as.character(workers_with_judgments$worker_id)
      #       h1 <- hPlot(judgments_count ~ worker_id,
      #                   data = workers_with_judgments, 
      #                   type = c("column"),
      #                   name="")
      
      
      workers_with_judgments$x = workers_with_judgments$worker_id # for proper ordering of bars
      workers_with_judgments$y = workers_with_judgments$judgments_count
      
      h1 <- rCharts::Highcharts$new()
      workers_with_judgments = lapply(split(workers_with_judgments, 
                                            1:nrow(workers_with_judgments)), as.list)
      names(workers_with_judgments) = NULL
      print("Line 907")
      print(workers_with_judgments[[1]])
      h1$series(data = workers_with_judgments, type = "column", name = "Workers")
      
      
      h1$plotOptions(
        series = list(
          cursor = 'pointer',
          events = list(
            click = 
              "#! function() { window.open(this.point.click_action); } !#"
          )
        )
      )
      
      
      h1$tooltip(useHTML = T, formatter = 
                   "#! function() { return('ID: ' + this.point.worker_id + '<br>' + 'Judgments: ' + this.point.y); } !#")
      #       
      #       h1$tooltip(useHTML = T, 
      #                  formatter = "#! function() { return('<b>' + 'ID: ' + this.point.x '</b><br>' + this.point.y + ' judgments'); } !#")
      #       
      #       
      # remove the x axis and the series name
      h1
    }
  })
  
  output$working_summary <- renderText({
    if (input$get_job == 0  || input$job_id == 0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      line1 = "<div class=\"bar_divs well\" id=\"working_div\" style=\"display: none;\">"
      title ="<h4>Workers In This Task</h4>"
      workers_with_judgments = pull_judgment_counts()
      num_workers = nrow(workers_with_judgments)
      num_judgments = sum(workers_with_judgments$judgments_count)
      ##### the graph
      h1 = num_judgments_bars()
      # you need to insert this as html/script, so the chart needs to be processed properly first
      chart_html = paste(capture.output(h1$show('inline')), collapse="")
      #####
      overview = paste0("There are ", num_workers, 
                        " workers who can still do work in this job.<br>",
                        "They made ", num_judgments, " judgments so far.")
      recommendation = ""
      closing_div = "</div>"
      ###### the price piece ends
      ## complexity will potentially be here too
      summary = paste(line1, title, overview, chart_html,
                      closing_div, sep="<br>")
      paste(summary)
    }
  })
  
})
