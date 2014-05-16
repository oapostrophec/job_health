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
             #tableOutput("jobData"),
             #tableOutput("numJobsAvailable"),
             #tableOutput("payrateSatisfaction")
             ),
    tabPanel("Quality Analysis",
             tableOutput("worksetData")),
    tabPanel("Detected Job Flaws")
  ))
))



