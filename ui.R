### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

library(shiny)

# shiny UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Fund Database"),
  
  sidebarPanel(
    radioButtons(inputId="filter_choice",label="Filter by Fund or Date Range?",choices=c("Fund","Date Range"),selected="Fund"),
    # byDate panel
    conditionalPanel(
      condition = "input.filter_choice == 'Date Range'",
      selectInput(inputId="byDate_start_date",label="Start Date:",choices=unique(sort(z$date)),selected="2007-01-31"),
      selectInput(inputId="byDate_end_date",label="End Date:",choices=rev(unique(sort(z$date)))),
      uiOutput("byDate_choose_fund")
    ),
    # byFund panel
    conditionalPanel(
      condition = "input.filter_choice == 'Fund'",
      selectInput(inputId="byFund_select_fund",label="Select Fund:",choices=unique(sort(z$fund)),selected="HFRI Fund Weighted Composite Index"),
      uiOutput("byFund_start_date"),
      uiOutput("byFund_end_date")
    ),
    textInput(inputId="correlation_min",label="Minimum Correlation:",value=""),
    helpText(HTML("<br>*Created by: <a href = \"https://twitter.com/jfreels4\">@jfreels4</a>
                  <br>*github <a href = \"https://github.com/jfreels/fund_database\">code</a>
                  ")
             )
  ),
  mainPanel(
    
    tabsetPanel(
      #tabPanel("test",
      #         verbatimTextOutput("test")
      #),
      tabPanel("Data Table",
               tableOutput("table")
      ),
      tabPanel("Track Record",
               plotOutput("returns_chart"),
               tableOutput("returns_table")
      ),
      tabPanel("Drawdowns",
               plotOutput("drawdowns_chart"),
               verbatimTextOutput("drawdowns_table")
      ),
      tabPanel("Fund List",
               tableOutput("manager_dates")
      )#,
      #tabPanel("Original Data",
      #         verbatimTextOutput("dataOriginal")
      #)
    )
  )
))