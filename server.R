library(shiny)
library(ggplot2)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  dataInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote, fileEncoding = input$encoding)
    
    # Transform the date columns (the ones between the Opened and the Closed column including both) to date
    dateCols <- which(colnames(data)==input$columnOpened):which(colnames(data)==input$columnClosed) 
    for (idx in dateCols) {
      data[,idx] <- as.Date(data[,idx], format=input$dateFormat)
    }
    
    # Compute lead time and the closing month and year (used for aggregation) for each row
    data[,"LeadTime"] <- as.numeric(data[,tail(dateCols, 1)] - data[,head(dateCols, 1)])
    data[,"ClosedMonth"] <- strftime(data[,tail(dateCols, 1)], "%m")
    data[,"ClosedYear"] <- strftime(data[,tail(dateCols, 1)], "%Y")
    
    # Remove outliers from the lead time according to user prefs
    qnt <- quantile(data[,"LeadTime"], probs=c(0, .25, .50, .75, .85, .95, 1), na.rm = T)
    H <- 1.5 * IQR(data[,"LeadTime"], na.rm = T)
    
    # Exclude outliers based on user input 
    data[data[,"LeadTime"] < (qnt["25%"] - H),"LeadTime"] <- NA
    switch(input$outliers,
           ninetyfive = data[data[,"LeadTime"] > qnt["95%"], "LeadTime"] <- NA,
           iqr = data[data[,"LeadTime"] > (qnt["75%"] + H), "LeadTime"] <- NA,
           none = time)
    
    # Return data, quantiles and date columns
    list(data=data, qnt=qnt, dateCols=dateCols)
  })
  
  # Data table parsed from the input file
  output$data <- renderDataTable({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    data <- input$data
    d <- dim(data)[2]
    data[,1:(d-2)]
  })
  
  # Cumulative Flow Diagram for all the date columns
  output$cfd <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    data <- input$data
    dateCols <- input$dateCols
    names(dateCols) <- names(data)[dateCols]
    
    dfs <- lapply(dateCols, function(x) { 
      t <- table(data[,x])
      cs <- cumsum(t)
      df <- data.frame(Date = sort(unique(data[,x])))
      df$Items <- cs
      names(df) <- c("Date", "Items")
      df
    })
    
    p <- ggplot()
    
    for (idx in seq_along(dfs)) {
      fill <- names(dfs)[idx]
      df <- dfs[[idx]]
      p <- p + geom_area(data=df, aes_q(quote(Date), quote(Items), fill=fill))
    }
    
    p <- p + scale_fill_brewer(palette = "Paired") + theme_bw() + theme(legend.title=element_blank())
      
    print(p)
  })
  
  # Lead Time Hstogram with vertical lines at different quantiles
  output$histo <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[,"LeadTime"]
    qnt <- input$qnt
    hvals <- hist(time, breaks=length(unique(time)), xlim=c(qnt["0%"], max(time, na.rm=TRUE)))
    top <- hvals$counts[1]
    abline(v = qnt["50%"], col = "goldenrod1", lwd = 1, lty = 2)
    text(x = qnt["50%"], top, col = "goldenrod1", labels="50%", pos=4)
    abline(v = qnt["75%"], col = "orange", lwd = 1, lty = 2)
    text(x = qnt["75%"], top, col = "orange", labels="75%", pos=4)
    abline(v = qnt["95%"], col = "red", lwd = 1, lty = 2)
    text(x = qnt["95%"], top, col = "red", labels="95%", pos=2)
  })
  
  # Lead Time Scatter Plot with horizontal lines at different quantiles
  output$scatter <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[,"LeadTime"]
    data <- input$data
    qnt <- input$qnt
    plot(data[,"Closed"], time, xlab="Closed date", col=rgb(0,100,0,50,maxColorValue=255), pch=16, xaxt  = "n")
    dmin <- min(data[,"Closed"], na.rm=T)
    dmax <- max(data[,"Closed"], na.rm=T)
    ddif <- dmax - dmin
    ticks_at <- dmin + approx(c(0, 503), n=4)$y
    axis(side=1, at=ticks_at, ticks_at)
    abline(h = qnt["50%"], col = "goldenrod1", lwd = 1, lty = 2)
    text(dmin, qnt["50%"], col = "goldenrod1", labels="50%", pos=4)
    abline(h = qnt["75%"], col = "orange", lwd = 1, lty = 2)
    text(dmin, qnt["75%"], col = "orange", labels="75%", pos=4)
    abline(h = qnt["95%"], col = "red", lwd = 1, lty = 2)
    text(dmin, qnt["95%"], col = "red", labels="95%", pos=4)
  })
  
  # Evolution of the Lead Time and Throughput over time
  output$evolution <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[,"LeadTime"]
    data <- input$data
    evolution <- aggregate(LeadTime ~ ClosedMonth + ClosedYear, data, FUN = function(x) c(meanLeadTime = mean(x), throughput = length(x) ) )
    months <- ISOdate(evolution[,"ClosedYear"], evolution[,"ClosedMonth"], 1)
    meanLeadTime <- evolution["LeadTime"][[1]][,"meanLeadTime"]
    throughput <- evolution["LeadTime"][[1]][,"throughput"]
    par(xpd=TRUE,oma=c(3,0,0,0)) 
    par(mar = c(5,4,4,4) + 0.1)
    barplot(throughput, col="gray",yaxt="n",xlab="",ylab="")
    axis(4)
    par(new=T)
    plot(months, meanLeadTime, type="o", col="red", xlab = "", ylab = "Mean lead time in days (lines)", lwd=2)
    mtext("Throughput in #tickets (bars)", side = 4, line = 3, cex = par("cex.lab"))
  })
})



