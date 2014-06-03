require('shiny')
require('devtools')
require('stringr')

shinyUI(pageWithSidebar(
  headerPanel("Job Heart Monitor"),
  sidebarPanel(
    h4("Enter a Job ID:"),
    HTML('<div class="row-fluid"><span class="span9">
     <input class=\"shiny-bound-input span9\" type=\"number\" id=\"job_id\" value=\"0\"></span>  
    <span class="span3"><button id="get_job" type="button" class="btn btn-info action-button shiny-bound-input">
    Submit</button></span></div>'),
    #div(h4("****OR****"), class="span6"),
    #div(h4("Enter an Account Email:"), class="span7"),
    #HTML('<div class=\"row-fluid\"><span class=\"span9\"><input class=\"shiny-bound-input span8\" type=\"text\" id=\"email\" value=\"who@what.com\"></span>
    # <span class=\"span3\"><button id=\"get_email\" type=\"button\" class=\"btn btn-info action-button shiny-bound-input\">
    # Submit</button></span></div>'),
    div(htmlOutput("job_summary_message"), class="span10"),
    div(htmlOutput("createJobHyperlink"), class="span10"),
    tags$style(type="text/css", "#spin { visibility: hidden; }"),
    #htmlOutput("accountSummary"),
    htmlOutput("renderLogo")),
  mainPanel(tabsetPanel(
    tabPanel("Throughput Analysis",
             div(htmlOutput("job_settings_message"), class="span9 alert alert-info"),
             div(htmlOutput("throughput_errors"), class="span10"),
             div(htmlOutput("throughput_warnings"), class="span10")
             #tableOutput("channelData"),
             #tableOutput("worksetData"),
             #tableOutput("numJobsAvailable")
             #tableOutput("payrateSatisfaction")
             #tableOutput("five_num_worker_table")
    ),
    tabPanel("Quality Analysis",
             div(htmlOutput("quality_gold_errors"), class="span11"),
             #div(htmlOutput("quality_times_warnings"), class="span11"),
             #div(htmlOutput("quality_cautions"), class="span11"),
             div(p("Pending Graph Idea on TQs"),
                 a("Missed TQs Bar Chart", target="_blank", href="http://www.highcharts.com/demo/column-rotated-labels/grid-light"), 
                 class="span6 well"),
             div(p("Pending Graph Idea Speed Density"), 
                 a("Density Graph of Submission Rates", target="_blank", href="http://www.highcharts.com/demo/area-basic/grid-light"),
                 class="span5 well"),
             div(p("Pending Graph Idea on Gold Value Distros"), 
                 a("Provided Gold Answers Bar Chart", target="_blank", href="http://www.highcharts.com/demo/column-rotated-labels/grid-light"),
                 class="span6 well"),
             div(p("Pending Graph Idea on Answer Distros"), 
                 a("Bar Chart Group by Gold Answers and Unit Answers", target="_blank", href="http://www.highcharts.com/demo/column-basic/grid-light"),
                 class="span5 well")
             ),
    tabPanel("Detected Job Flaws",
             div(htmlOutput("job_settings_warnings"), class="span10"),
             div(htmlOutput("job_settings_cautions"), class="span10"),
             div(htmlOutput("job_settings_overview"), class="span11 alert alert-info")
             #showOutput("throughput_bar", "highcharts")
             #showOutput("tainted_bar",  "highcharts"),
             #div(h4("Drill down for Tainted"), class="bar_divs", id="tainted_div", style="display:none")
             #,
             #div(h4("Drill down for Chacked Out"), class="bar_divs", id="checked_out_div", style="display:none")
    ),
    tabPanel("Throughput (THE BAR)",
             showOutput("throughput_bar", "highcharts"),
             #showOutput("tainted_bar",  "highcharts"),
             htmlOutput("maxed_out_summary"),
             htmlOutput("not_in_yet_summary"),
             htmlOutput("checked_out_summary"),
             htmlOutput("tainted_summary"),
             htmlOutput("working_summary")
             #,
             #div(h4("Drill down for Tainted"), class="bar_divs", id="tainted_div", style="display:none")
             #,
             #div(h4("Drill down for Chacked Out"), class="bar_divs", id="checked_out_div", style="display:none")
    )
  )
  )
))
