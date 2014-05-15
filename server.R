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
source('get_country_data.R')
source('get_channel_data.R')
#source('get_worker_data.R')
source('get_workset_data.R')

system('mkdir -p /tmp/job_health')

options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)
options(scipen=999)

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
  
  pull_country_data <- reactive({
    if(input$get_job == 0){
      return(NULL)
      #}else if(input$get_job != 0){
    }else{
      job_id = input$job_id
      if(job_id == 0){
        return(NULL)
      }else{
        data = get_country_data(job_id) 
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
        data = get_job_data(job_id) 
        print(head(data))
        data
      }
    }
  })
  
  get_skill_names <- reactive({
    if(input$get_job == 0){
      return(NULL)
    }else{
     skills_data = pull_job_data()
     min_require = skills_data$minimum_requirements
     skills_strip = str_replace_all(min_require, "(\\{skill_scores:\\{|\\}.+)", "") 
     skills_simple = str_replace_all(skills_strip, "(:\\d)", "")
     skill_split = str_split(skills_simple, ",")
     skill_unlist = unlist(skill_split[[1]])
     skill_unlist = skill_unlist[skill_unlist!=""]
     skills_for_psql = paste(paste0("'",skill_unlist,"'"), collapse=",")
     skills_for_psql
    }
  })
  
  get_skill_score <- reactive({
    if(input$get_job == 0){
      return(NULL)
    }else{
      skills_data = pull_job_data()
      min_require = skills_data$minimum_requirements
      score = str_extract(min_require, "(min_score:\\d)")
      score_num = str_extract(score, "\\d")
      score_num = as.double(score_num)
      score_num
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
  
})