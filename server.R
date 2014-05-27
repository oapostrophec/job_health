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
  
  pull_workset_data <- reactive({
    if(input$get_job == 0){
      return(NULL)
    }else{
      job_id = input$job_id
      if (job_id == 0) {
        return(NULL)
      }else{
        db = db_call
        query = get_workset_data(job_id)
        file = paste0(temp_dir,"/",
                      "workset", "_", job_id, "_",
                      format(Sys.time(), "%b_%d_%X_%Y"),
                      ".csv")
        data = run_this_query(db, query, file)
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
      responses_table_transformed = 
        data.frame(group=c("maxed out", "working","tainted","checked_out", "not_in_yet"),
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
      line1 = "<div class=\"bar_divs\" id=\"maxed_out_div\" style=\"display: none;\">"
      title ="<h4>Maxed Out</h4>"
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
      workset = pull_workset_data()
      workset = as.data.frame(workset)
      
      #Variables to Display
      job_title = json$title 
      job_state = json$state
      support_email = json$support_email
      num_gold_units = json$golds_count
      num_nongold_units = json$units_count - json$golds_count
      
      total_judgments = sum(workset$judgments_count - workset$golds_count)
      
      print("workset is:")
      print(head(workset))
      
      tainted_work = 
        workset[workset$tainted == "t",]
        
      untrusted_all = sum(tainted_work$judgments_count)
      untrusted_golds = sum(tainted_work$golds_count)
      
      untrusted_judgments = untrusted_all - untrusted_golds
      trusted_judgments = total_judgments - untrusted_judgments
      
      if (num_gold_units == 0) {
        gold_message = "<p style='color:red;'>It looks like there are NOT any <b>TQs</b> in this job. 
                        Unless this is a survey job or a doublepass job, 
                        they should use test questions.</p>"
      } else {
        gold_message = ""
      }
      
      overall_message = paste("<h5>Job Summary</h5>",
                              "<ul class=\"unstyled\"><li>Job Title:<br>", job_title, "</li><br>",
                              "<li>State: ", job_state, "</li>",
                              "<li>Email: ", support_email, "</li>",
                              "<li>Units: ", num_nongold_units, "</li>",
                              "<li>Test Questions: ", num_gold_units, "</li><br>",
                              "<li>Total Judgments: ", total_judgments, "</li>",
                              "<li>Untrusted Judgments: ", untrusted_judgments, "</li>",
                              "<li>Trusted Judgments: ", trusted_judgments, "</li>",
                              "</ul>", sep="")
      
      paste(overall_message, gold_message)
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
      if(id_flag == "t"){
        id_flag = "TRUE"
      } 
      
      if(id_flag == "f"){
        id_flag = "FALSE"
      }
      
      id_email = job$send_emails_on_rate_limit
      if(id_email == "t"){
        id_email = "TRUE"
      }
      
      if(id_email == "f"){
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
      fifo = job$schedule_fifo
      if(fifo == "t"){
        fifo = "TRUE"
      }
      
      if(fifo == "f"){
        fifo = "FALSE"
      }
      
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
  
  output$throughput_erros <- renderText({
    #if (input$get_job == 0 || input$job_id == 0) {
    # User has not uploaded a file yet
    #  return("<p>No job data to return.</p>")
    #} else {
    #json = get_job_settings_from_json()
    available = 99
    #get_worker_intersect()
    maxed = 20 
    #get_maxed_out()
    tainted = 10
    #get_tainted()
    qm_fail = 15
    #get_qm_failures()
    dropouts = 20
    #get_dropouts() 
    onlookers = 20
    #get_onlookers()
    viable = 10
    #get_viable() 
    
    #reject_at = json$options$reject_at
    if(available < 100 || is.na(available) || is.null(available)){
      too_small = "<p><i class=\"icon-minus-sign\"></i> <b>Hold Up: The contributor pool for this job is very small. 
      You need to consider broadening it (targetting more countries, levels, etc) 
      or resetting your throughput expectations.</b></p>"
    } else {
      too_small=""
    }
    
    total_worked = viable + maxed + tainted + qm_fail + dropouts + onlookers
    percent_viable = (viable/total_worked)*100
    percent_maxed = (maxed/total_worked) * 100
    percent_tainted = (tainted/total_worked) * 100
    percent_qm_fail = (qm_fail/total_worked) * 100
    percent_dropouts = (dropouts/total_worked) * 100
    percent_onlookers = (onlookers/total_worked) * 100
    
    if(percent_tainted + percent_qm_fail > 0){
      failure_message = "<p><i class=\"icon-remove-sign\"></i> Ah oh: We're getting a lot of failures in quiz and
      work mode. You may want to check on the Test Questions and the reject_at rate.</p>"
    } else {
      failure_message = ""
    }
    
    if(percent_maxed > 0){
      maxed_message = "<p><i class=\"icon-resize-full\"></i> Note: Over %50 of the workers in the job have maxed out. If the job is not near to finishing you may want to add more TQs or up the max work settings.</p>"
    } else {
      maxed_message = ""
    }
    
    if(too_small == "" && failure_message == "" && maxed_message == ""){
      paste("<p class=\"alert alert-success\">
             <i class=\"icon-ok\"></i>
             <big>Throughput Contributor Errors:</big>
             <br>We did not detect any obvious errors.</p>")
    } else {
      paste("<div class=\"alert alert-error\">", "<p><big>Throughput Contributor Errors:</big></p>",
            too_small, failure_message, maxed_message, "</div>")
    }
    #}
    
  })
  
  output$throughput_warnings <- renderText({
    #if (input$get_job == 0 || input$job_id == 0) {
    # User has not uploaded a file yet
    #  return("<p>No job data to return.</p>")
    #} else {
    
    #json = get_job_settings_from_json()
    available = 99
    #get_worker_intersect()
    maxed = 20 
    #get_maxed_out()
    tainted = 10
    #get_tainted()
    qm_fail = 15
    #get_qm_failures()
    dropouts = 20
    #get_dropouts() 
    onlookers = 20
    #get_onlookers()
    viable = 10
    #get_viable() 
    
    total_worked = viable + maxed + tainted + qm_fail + dropouts + onlookers
    percent_viable = (viable/total_worked)*100
    percent_maxed = (maxed/total_worked) * 100
    percent_tainted = (tainted/total_worked) * 100
    percent_qm_fail = (qm_fail/total_worked) * 100
    percent_dropouts = (dropouts/total_worked) * 100
    percent_onlookers = (onlookers/total_worked) * 100
    
    if(percent_viable < 20){
      viable_message = "<p>Careful: Looks like your group of active contributors is dwindling.</p>"
    } else {
      viable_message = ""
    }
    
    if(percent_dropouts + percent_onlookers > 35){
      lookers_message = "<p>Ah oh: We're seeing a high percentage of contributors just looking at the task or giving up after quiz mode. You may want to up the Payment per Task or broaden your contributor target.</p>"
    } else {
      lookers_message = ""
    }
    
    if(viable_message == "" && lookers_message == ""){
      paste("<p class=\"alert alert-success\">
             <i class=\"icon-ok\"></i>
             <big>Throughput Contributor Warnings:</big>
             <br>We did not detect any obvious errors.</p>")
    } else {
      paste("<div class=\"alert\">", "<p><big>Throughput Contributor Cautions:</big></p>",
            viable_message, lookers_message, "</div>")
    }
    #}
  })
  
  output$quality_gold_errors <- renderText({
    #if (input$get_job == 0 || input$job_id == 0) {
    # User has not uploaded a file yet
    #  return("<p>No job data to return.</p>")
    #} else {
    
    i = 4
    j = 5
    
    if(i < j){
      tq_message = "<p><i class=\"icon-edit\"></i> Eek: There are quite a few test questions that are highly missed. 
      We would update those before digging into Quality too much.</p>"
    } else {
      tq_message = ""
    }
    
    #Enough Golds
    #wrt number of units for every 100 units there should be AT LEAST 10 units.
    if(i < j){
      enough_golds_message = "<p><i class=\"icon-list-alt\"></i> Careful: There are very few golds given the number of units. 
      You may want to increase it.</p>"
    } else {
      enough_golds_message = ""
    }
    
    #Bad Validators
    #If a gold answer does not match the validators (for text) or names/values (for non text)
    if(i < j){
      validators_message = "<p><i class=\"icon-fire\"></i><big> WARNING! We detected that some of the answers provided in TQs DO NOT match the values provided in CML. 
      Please pause and fix this before continuing.</big><p>"
    } else {
      validators_message = ""
    }
    if(tq_message == "" && enough_golds_message == "" && validators_message == ""){
      paste("<p class=\"alert alert-success\">
              <i class=\"icon-ok\"></i>
              <big>Gold Quality Warnings:</big>
              <br>We did not detect any obvious errors.</p>")
    } else {
      paste("<div class=\"alert alert-error\">", "<p><big>Gold Quality Warnings:</big></p>",
            validators_message, tq_message, enough_golds_message, "</div>")
    }
    #}
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
  
})
