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
    div(htmlOutput("renderLogo"), class="span5"),
    #div(h4("Enter an Account Email:"), class="span7"),
    #HTML('<div class=\"row-fluid\"><span class=\"span9\"><input class=\"shiny-bound-input span8\" type=\"text\" id=\"email\" value=\"who@what.com\"></span>
    # <span class=\"span3\"><button id=\"get_email\" type=\"button\" class=\"btn btn-info action-button shiny-bound-input\">
    # Submit</button></span></div>'),
    htmlOutput("jobSummary"),
    htmlOutput("accountSummary")),
  mainPanel(tabsetPanel(
    tabPanel("Throughput Analysis",
             div(h4("Alerts"), 
                 HTML('<ul>
                       <li>Alert for Small Worker Pool</li>
                       <li>Low Pay Alert</li>
                       <li>Lost &amp; Stopped Work Alert</li>
                       </ul>'), class="span6 well"),
             div(h4("Bar Charts here for available workers & current worker breakdowns"), class="span5")
             #tableOutput("channelData"),
             #tableOutput("worksetData"),
             #,
             #tableOutput("numJobsAvailable"),
             #tableOutput("payrateSatisfaction")
             #tableOutput("five_num_worker_table")
    ),
    tabPanel("Quality Analysis",
             tableOutput("worksetData")),
    tabPanel("Detected Job Flaws",
             showOutput("throughput_bar", "highcharts"),
             htmlOutput("maxed_out_summary1"),
             tags$div(class="bar_divs", id="maxed_out_div", style="display:none",
               tags$h4("Drill down for Maxed Out"),
               textOutput("maxed_out_summary")
               ),
             div(h4("Drill down for Working"), class="bar_divs", id="working_div", style="display:none"),
             div(h4("Drill down for Tainted"), class="bar_divs", id="tainted_div", style="display:none"),
             div(h4("Drill down for Chacked Out"), class="bar_divs", id="checked_out_div", style="display:none"),
             div(h4("Drill down for Not In Yet"), class="bar_divs", id="not_in_yet_div", style="display:none")
    )
  )
  )
))
