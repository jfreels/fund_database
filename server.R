### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

library(shiny)

### Shiny Server
shinyServer(function(input, output) {
  
  # reactive: datesOriginal
  # dates in Original dataset
  datesOriginal<-reactive({
    datesOriginal<-ddply(z,.(fund),summarise,
                         start=as.character(min(date)),
                         end=as.character(max(date)))
  })
  
  # output: dataOriginal
  output$dataOriginal<-renderPrint({
    z
  })
  
  ### byDate REACTIVES
  # reactive: datesReactive
  datesReactive<-reactive({
    subset(datesOriginal(),start<=input$byDate_start_date&end>=input$byDate_end_date)
  })
  
  # reactive: datesReactiveNames
  # dataset fund names
  datesReactiveNames<-reactive({
    droplevels(datesReactive()$fund)
  })
  
  # uiOutput: byDate_choose_fund
  output$byDate_choose_fund<-renderUI({
    selectInput(inputId="byDate_fund",label="Choose Fund:",choices=unique(datesReactiveNames()))
  })
  
  # reactive: byFund_dataset
  byFund_dataset<-reactive({
    dat<-subset(z,date>=ymd(input$byFund_start_date2)&date<=ymd(input$byFund_end_date2))
    datPlyr<-ddply(dat,.(fund),"nrow")
    datPlyr<-subset(datPlyr,nrow==max(datPlyr$nrow))
    subset(dat,fund %in% datPlyr$fund)
  })
  
  # reactive: byDate_dataset
  byDate_dataset<-reactive({
    dat<-subset(z,fund %in% datesReactiveNames())
    dat<-subset(dat,date>=ymd(input$byDate_start_date))
    dat<-subset(dat,date<=ymd(input$byDate_end_date))
    dat
  })
  
  # reactive: dataset
  # filtered by start_date and end_date
  dataset<-reactive({
    if (input$filter_choice=="Fund") {
      byFund_dataset()
    } else {
      byDate_dataset()
    }
  })
  
  # reactive: datasetStats
  datasetStats<-reactive({
    stats<-ddply(dataset(),.(fund),summarise,
                 cROR=cror(return)*100,
                 aROR=aror(return)*100,
                 aSD=asd(return)*100,
                 Sharpe=sharpe(return),
                 MaxDD=maxdd(return)*100,
                 Omega=omega(return),
                 Start=as.character(min(date)),
                 End=as.character(max(date)))
    stats
  })
  
  # reactive: datasetCorrelations
  datasetCorrelations<-reactive({
    fundName<-if(input$filter_choice=="Fund") {
      input$byFund_select_fund
    } else { input$byDate_fund }
    matrix<-na.omit(dcast(dataset(),date~fund,value.var="return"))
    corMatrix<-round(cor(matrix[,-1]),2)
    corMatrixMelted<-melt(corMatrix)
    corTable<-arrange(subset(corMatrixMelted,Var2 %in% fundName),-value)
    names(corTable)<-c("fund","itself","correlation")
    corTable[,-2]
  })  
  
  # reactive: mgr
  # subset of total manager track record
  mgr<-reactive({
    if (input$filter_choice=="Fund") {
      dat<-subset(z,fund==input$byFund_select_fund)
      dat<-subset(dat,date>=ymd(input$byFund_start_date2)&date<=ymd(input$byFund_end_date2))
      dat
    } else {
      dat<-subset(z,fund==input$byDate_fund)
      dat<-subset(dat,date>=ymd(input$byDate_start_date)&date<=ymd(input$byDate_end_date))
      dat
    }    
  })
  
  # reactive: mgrXts
  # subset of total manager track record in Xts
  mgrXts<-reactive({
    dat<-xts(mgr()[,3],mgr()[,1])
    dat
  })
  
  ### byFund REACTIVES
  
  byFund_date_range<-reactive({
    dat<-subset(z,fund==input$byFund_select_fund)
    unique(dat$date)
  })
  
  # uiOutput: byFund_start_date
  output$byFund_start_date<-renderUI({
    selectInput(inputId="byFund_start_date2",label="Choose Start Date:",choices=byFund_date_range())
  })
  # uiOutput: by_Fund_end_date 
  output$byFund_end_date<-renderUI({
    selectInput(inputId="byFund_end_date2",label="Choose End Date:",choices=rev(byFund_date_range()))
  })
    
  # output: test
  output$test<-renderPrint({
    head(dataset(),10)
  })
  
  ### OUTPUTS
  
  # output: table
  # display the correlations and stats
  output$table<-renderTable({
    table<-join(datasetCorrelations(),datasetStats(),by="fund")
    table<-arrange(table,-correlation)
    subset(table,correlation>=input$correlation_min)
  })
    
  # output: returns_chart
  output$returns_chart<-renderPlot({
    chart.CumReturns(mgrXts(),wealth.index=TRUE)
  })
  
  # output: returns_table
  output$returns_table<-renderTable({
    dat<-mgr()
    dat<-dcast(dat,year(date)~month(date))
    names(dat)<-c("Year",month.abb)
    dat<-arrange(dat,-Year)
    row.names(dat)<-dat[,1]
    dat<-dat[,-1]
    dat$Year<-apply(dat,1,aror)
    dat<-round(dat*100,2)
    dat
  })  
  
  # output: drawdowns_chart
  output$drawdowns_chart<-renderPlot({
    chart.Drawdown(mgrXts())
  })
  
  # output: drawdowns_table
  output$drawdowns_table<-renderPrint({
    table.Drawdowns(mgrXts(),top=100)
  })
    
  # output: manager_dates
  # display the managers and their start/end dates in the original data set
  output$manager_dates<-renderTable({
    datesOriginal()
  })
  
})