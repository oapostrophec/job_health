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
source('find_cml_elements.R')
source('velocity_violations_query_function.R')
source('answer_flags_query_function.R')
source('all_taints_query_function.R')
source('trust_taints_query_function.R')
source('workers_with_judgments_query_function.R')
source('everyone_available_query_function.R')
source('workers_with_no_judgments_query.R')
source('worker_stats_query_function.R')


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
      print("line 118")
      print("Workset server Line 1117")
      print(names(data))
      print(head(data))
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
  
  
  #   states = c("maxed out", "working",
  #              "tainted","checked_out", "not_in_yet")
  get_state_counts<- reactive({
    if (input$get_job == 0 || input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      maxed_out = as.numeric(get_num_maxed_out())
      print(310)
      working = nrow(pull_judgment_counts())
      print(312)
      tainted = sum(pull_tainted_breakdown_data()$y)
      print(314)
      checked_out = as.numeric(get_number_checked_out())
      print(316)
      all_available_workers = as.numeric(get_everyone_available())
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
      
      
      #workers_with_judgments$worker_id = as.character(workers_with_judgments$worker_id)
      #       h1 <- hPlot(judgments_count ~ worker_id,
      #                   data = workers_with_judgments, 
      #                   type = c("column"),
      #                   name="")
      
      workers_with_judgments$x = workers_with_judgments$index # for proper ordering of bars
      workers_with_judgments$y = workers_with_judgments$judgments_count
      
      h1 <- rCharts::Highcharts$new()
      workers_with_judgments = lapply(split(workers_with_judgments, 
                                            rownames(workers_with_judgments)), as.list)
      names(workers_with_judgments) = NULL
      print("Line 907")
      print(workers_with_judgments[[1]])
      h1$series(data = workers_with_judgments, type = "column", name = "Workers")
      
      
      h1$plotOptions(
        series = list(
          cursor = 'pointer',
          events = list(
            click = 
              "#! function() { window.open(this.options.data[0].click_action); } !#"
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
