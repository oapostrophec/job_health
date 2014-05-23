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
    #div(htmlOutput("renderLogo"), class="span6"),
    htmlOutput("accountSummary")),
  mainPanel(tabsetPanel(
    tabPanel("Throughput Analysis",
             div(h4("Alerts"), 
                 HTML('<ul>
                       <li>Alert for Small Worker Pool</li>
                       <li>Low Pay Alert</li>
                       <li>Lost &amp; Stopped Work Alert</li>
                       </ul>'), class="span6 well"),
             div(h4("Bar Charts here for available workers & current worker breakdowns"), class="span5"),
             #tableOutput("channelData"),
             #tableOutput("worksetData"),
             #,
             tableOutput("numJobsAvailable")
             #tableOutput("payrateSatisfaction")
             #tableOutput("five_num_worker_table")
    ),
    tabPanel("Quality Analysis"),
    tabPanel("Detected Job Flaws"),
    tabPanel("Throughput (caution:runs slowly)",
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
