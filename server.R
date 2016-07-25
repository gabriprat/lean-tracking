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
    
    closedIdx <- data[,input$columnState] == input$closedState 
    
    # Remove inconsistencies: open tasks with closed date
    openIdx <- !(data[,input$columnState] %in% c(input$closedState, input$discardedState))
    data[openIdx, input$columnClosed] <- NA 
    
    # Compute lead time and the closing month and year (used for aggregation) for each row
    data[closedIdx,"LeadTime"] <- as.numeric(data[closedIdx,tail(dateCols, 1)] - data[closedIdx,head(dateCols, 1)])
   
     #Remove negative lead times
    #data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] < 0,"LeadTime"] <- NA 
    
    cm <- strftime(data[,tail(dateCols, 1)], "%m")
    cy <- strftime(data[,tail(dateCols, 1)], "%Y")
    data[,"ClosedMonth"] <- ISOdate(cy, cm, 1)
    
    # Remove outliers from the lead time according to user prefs
    qnt <- quantile(data[closedIdx,"LeadTime"], probs=c(0, .25, .50, .75, .85, .95, 1), na.rm = T)
    H <- 1.5 * IQR(data[closedIdx,"LeadTime"], na.rm = T)
    
    # Exclude outliers based on user input 
    data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] < (qnt["25%"] - H),"LeadTime"] <- NA
    switch(input$outliers,
           ninetyfive = data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] > qnt["95%"], "LeadTime"] <- NA,
           iqr = data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] > (qnt["75%"] + H), "LeadTime"] <- NA,
           none = time)
    
    # Return data, quantiles and date columns
    list(data=data, qnt=qnt, dateCols=dateCols, closedIdx=closedIdx, columnType=input$columnType)
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
    time <- input$data[input$closedIdx,"LeadTime"]
    qnt <- input$qnt
    hvals <- hist(time, breaks=length(unique(time)), xlim=c(qnt["0%"], max(time, na.rm=TRUE)))
    top <- hvals$counts[1]
    abline(v = qnt["50%"], col = "goldenrod1", lwd = 1, lty = 2)
    text(x = qnt["50%"], top, col = "goldenrod1", labels="50%", pos=4)
    abline(v = qnt["85%"], col = "orange", lwd = 1, lty = 2)
    text(x = qnt["85%"], top, col = "orange", labels="85%", pos=4)
    abline(v = qnt["95%"], col = "red", lwd = 1, lty = 2)
    text(x = qnt["95%"], top, col = "red", labels="95%", pos=2)
  })
  
  # Lead Time Scatter Plot with horizontal lines at different quantiles
  output$scatter <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[input$closedIdx,"LeadTime"]
    data <- input$data[input$closedIdx,]
    qnt <- input$qnt
    dateCols <- input$dateCols
    plot(data[,tail(dateCols, 1)], time, xlab="Closed date", col=rgb(0,100,0,50,maxColorValue=255), pch=16, xaxt  = "n")
    dmin <- min(data[input$closedIdx,tail(dateCols, 1)], na.rm=T)
    dmax <- max(data[input$closedIdx,tail(dateCols, 1)], na.rm=T)
    ddif <- dmax - dmin
    ticks_at <- dmin + approx(c(0, 503), n=4)$y
    axis(side=1, at=ticks_at, ticks_at)
    abline(h = qnt["50%"], col = "goldenrod1", lwd = 1, lty = 2)
    text(dmin, qnt["50%"], col = "goldenrod1", labels="50%", pos=4)
    abline(h = qnt["85%"], col = "orange", lwd = 1, lty = 2)
    text(dmin, qnt["85%"], col = "orange", labels="85%", pos=4)
    abline(h = qnt["95%"], col = "red", lwd = 1, lty = 2)
    text(dmin, qnt["95%"], col = "red", labels="95%", pos=4)
  })
  
  # Throughput evolution over time
  output$throughput <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[input$closedIdx,"LeadTime"]
    data <- input$data[input$closedIdx,]
    
    p <- ggplot(data,aes_string(x="ClosedMonth",y="1", fill=input$columnType)) +  
      stat_summary(fun.y=sum, position="stack", geom="bar") + 
      stat_summary(aes(label=..y..), position="stack", fun.y=length, geom="text", vjust = -.25) +
      theme_bw() + ylab("Work items") + ggtitle("Throughput")
    
    print(p)
    
  })
  
  # Lead time evolution over time
  output$leadTime <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[input$closedIdx,"LeadTime"]
    data <- input$data[input$closedIdx,]
    
    #stat_summary(fun.y=function(x) {quantile(x, .85)}) + 
    
    pctl85 <- function(x) {
      quantile(x, .85)
    }
    sd.ev <- do.call(data.frame, aggregate(LeadTime ~ ClosedMonth, data, FUN = function(x) { 
      x.mean <- mean(x)
      x.sd <- sd(x)
      c(mean = x.mean, sd = x.sd) 
    }))
    
    p <- ggplot(data,aes(x=ClosedMonth, y=LeadTime)) +  
      stat_summary(geom="ribbon", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), fill="lightblue")+
      stat_summary(aes(colour="pctl85", shape="pctl85", group=1, label=..y..), fun.y=pctl85, geom="line", size=1.1) +
      stat_summary(aes(colour="mean", shape="mean", group=1, label=..y..), fun.y=mean, geom="line", size=0.7, linetype="dashed") +
      stat_summary(aes(label= round(..y..), colour="pctl85"), fun.y=pctl85, geom="text", position=position_dodge(.9), vjust = -1.5, show.legend = FALSE) +
      geom_text(aes(x=ClosedMonth, y=LeadTime.mean, label=paste0(round(LeadTime.mean), "±", round(LeadTime.sd)), colour="mean", size=.7, parse=T), data=sd.ev, position=position_dodge(.9), vjust = -1.5, show.legend = FALSE) +
      theme_bw() + scale_colour_manual(values = c("#377EB8", "#E41A1C")) + 
      ylab("85%ile lead time") + ggtitle("Lead time") + theme(legend.title=element_blank(), legend.key = element_blank())
    
    print(p)
    
  })
})



