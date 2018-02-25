library(ggplot2)
library(gtable)
library(xts)
library(zoo)
library(plotly)
library(RColorBrewer)
library(stringr)

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
    
    data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE,
             sep = input$sep, quote = input$quote, encoding = input$encoding)
    
    if (!input$columnOpened %in% names(data)) {
      stop(paste0('The opened column "', input$columnOpened, '" is not present in the dataset. Found these columns: ', paste(names(data), collapse=", ")))
    }
    
    if (!input$columnClosed %in% names(data)) {
      stop(paste0('The closed column "', input$columnClosed, '" is not present in the dataset. Found these columns: ', paste(names(data), collapse=", ")))
    }
    
    if (nchar(input$columnState) > 0 & !input$columnState %in% names(data)) {
      stop(paste0('The state column "', input$columnState, '" is not present in the dataset. Found these columns: ', paste(names(data), collapse=", ")))
    }
    
    if (nchar(input$columnType) > 0 & !input$columnType %in% names(data)) {
      stop(paste0('The type column "', input$columnType, '" is not present in the dataset. Found these columns: ', paste(names(data), collapse=", ")))
    } else {
      if (input$columnType %in% names(data)) {
        levels(data[,input$columnType]) <- c(levels(data[,input$columnType]), "<blank>")
        data[data[,input$columnType]=='',input$columnType] <- '<blank>'
      }
    }
      
    # Transform the date columns (the ones between the Opened and the Closed column including both) to POSIXct
    dateCols <- which(colnames(data)==input$columnOpened):which(colnames(data)==input$columnClosed) 
    for (idx in dateCols) {
      d <- as.POSIXct(strptime(data[,idx], format=input$dateFormat))
      data[,idx] <- d
    }
    
    repeat.before = function(x) {   # repeats the first non NA value. Keeps trailing NA
      x <- rev(x)
      ind = which(!is.na(x))          # get positions of nonmissing values
      if(is.na(x[1]))                 # if it ends with a missing, add the 
        ind = c(1,ind)                # first position to the indices
      x <- rep(x[ind], times = diff(  # repeat the values at these indices
        c(ind, length(x) + 1) ))      # diffing the indices + length yields how often 
                                      # they need to be repeated
      rev(x)  
    }                               
    
    for (i in 1:dim(data)[1]) {
      data[i,dateCols] <- repeat.before(data[i,dateCols])
    }
    
    closedIdx <- !is.na(data[,input$columnClosed])
    openIdx <- !closedIdx
      
    discardedIdx <- NULL
    if (input$columnState %in% names(data)) {
      closedIdx <- data[,input$columnState] == input$closedState 
      discardedIdx <- data[,input$columnState] == input$discardedState
      
      # Remove inconsistencies: open tasks with closed date
      openIdx <- !(data[,input$columnState] %in% c(input$closedState, input$discardedState))
      data[openIdx, input$columnClosed] <- NA 
    }
    
    # Compute lead time, age and the closing month and year (used for aggregation) for each row
    data[closedIdx,"LeadTime"] <- as.numeric(difftime(data[closedIdx,tail(dateCols, 1)], data[closedIdx,head(dateCols, 1)]), units = "days")
    data[openIdx,"Age"] <- as.numeric(difftime(Sys.Date(),  data[openIdx,head(dateCols, 1)]), units = "days")
    
    # Transform the date columns (the ones between the Opened and the Closed column including both) to Date
    for (idx in dateCols) {
      data[,idx] <- as.Date(data[,idx])
    }
    
    #Remove negative lead times
    data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] < 0,"Error"] <- TRUE
    data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] < 0,"LeadTime"] <- NA 
    
    cm <- strftime(data[,tail(dateCols, 1)], "%m")
    cy <- strftime(data[,tail(dateCols, 1)], "%Y")
    data[,"ClosedMonth"] <- ISOdate(cy, cm, 1)
    
    # Remove outliers from the lead time according to user prefs
    qnt <- quantile(data[closedIdx,"LeadTime"], probs=c(0, .25, .50, .75, .85, .95, 1), na.rm = T)
    H <- 1.5 * IQR(data[closedIdx,"LeadTime"], na.rm = T)
    
    data.all <- data[,]
    # Exclude outliers based on user input 
    data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] < (qnt["25%"] - H),"LeadTime"] <- NA
    switch(input$outliers,
           ninetyfive = data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] > qnt["95%"], "LeadTime"] <- NA,
           iqr = data[!is.na(data[,"LeadTime"]) & data[,"LeadTime"] > (qnt["75%"] + H), "LeadTime"] <- NA,
           none = time)
    
    # Return data, quantiles and date columns
    list(data=data, qnt=qnt, dateCols=dateCols, closedIdx=closedIdx, openIdx=openIdx, discardedIdx=discardedIdx, columnType=input$columnType, data.all=data.all)
  })
  
  # Data table parsed from the input file
  output$data <- renderDataTable({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    data <- input$data
    d <- dim(data)[2]
    data[,1:(d-1)]
  })
  
  # Cumulative Flow Diagram for all the date columns
  output$cfd <- renderDygraph({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    data <- input$data
    dateCols <- input$dateCols
    names(dateCols) <- names(data)[dateCols]
    
    dfs <- Reduce(function(acc, x) { 
      t <- table(data[,x])
      cs <- cumsum(t)
      df <- data.frame(Date = sort(unique(data[,x])))
      df[,names(data)[x]] <- cs
      
      merge(acc, df, by="Date", all=TRUE)
    }, dateCols, data.frame(Date=as.Date(character())))
    dfs <- xts(dfs[,-1], order.by = dfs[,1])
    # Start every column at 0
    dfs[1,is.na(dfs[1,])] <- 0
    # Interpolate values at NAs
    dfs <- na.locf(dfs)
    # Fill last NA values with previous ones
    dfs <- na.locf(dfs)
    
    sum <- rep(0, dim(dfs)[1])
    for (i in (length(dateCols)-1):1) {
      sum <- sum + dfs[,names(dateCols)[i+1]]
      dfs[,names(dateCols)[i]] <- dfs[,names(dateCols)[i]] - sum
    }
    
    p <- dygraph(dfs) %>% dyRangeSelector()
    
    for (serieName in names(dateCols)) {
      p <- p %>% dySeries(serieName, fillGraph = TRUE)
    }
    if (length(dateCols) > 12) {
      cl <- colorRampPalette(brewer.pal(12,"Paired"))(length(dateCols))
    } else {
      cl <- brewer.pal(12,"Paired")[1:length(dateCols)]
    }
    p <- p %>% dyOptions(stackedGraph=T, fillAlpha=.5, colors=cl) %>% 
      dyLegend(show = "always", hideOnMouseOut = FALSE, labelsDiv="cfd-labels", labelsSeparateLines=T)
    p
})
  
  # Lead Time Hstogram with vertical lines at different quantiles
  output$histo <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[input$closedIdx,"LeadTime"]
    qnt <- input$qnt
    hvals <- hist(time, breaks=length(unique(time)), xlim=c(qnt["0%"], max(time, na.rm=TRUE)), main="", xlab="Days to complete")
    top <- hvals$counts[1]
    abline(v = qnt["50%"], col = "goldenrod1", lwd = 1, lty = 2)
    text(x = qnt["50%"], top, col = "goldenrod1", labels="50%", pos=4)
    abline(v = qnt["85%"], col = "orange", lwd = 1, lty = 2)
    text(x = qnt["85%"], top, col = "orange", labels="85%", pos=4)
    abline(v = qnt["95%"], col = "red", lwd = 1, lty = 2)
    text(x = qnt["95%"], top, col = "red", labels="95%", pos=2)
  })
  
  # Lead Time Scatter Plot with horizontal lines at different quantiles
  output$scatter <- renderPlotly({
    input <- dataInput()
    nm <- names(data)[1]
    data <- input$data[input$closedIdx,]
    data[,"ID"] <- apply(data, 1, function(x) { paste0(nm, ": ", x[1]) }) 
    qnt <- input$qnt
    dateCols <- input$dateCols
    dmin <- min(data[,tail(dateCols, 1)], na.rm=T)
    dmax <- max(data[,tail(dateCols, 1)], na.rm=T)
    ddif <- dmax - dmin
    breaks <- seq(dmin, dmax, length.out=10)

    p <- ggplot(data, aes_string(x = names(data)[tail(dateCols, 1)], y = "LeadTime")) + 
      geom_point(aes_string(text="ID", colour = input$columnType), alpha = 0.3) +
      geom_hline(yintercept = qnt["50%"], colour = "goldenrod1", linetype=2, size=.25) + 
      annotate("text", x=dmin, y = qnt["50%"], colour = "goldenrod1", label="50%") +
      geom_hline(yintercept = qnt["85%"], colour = "orange", linetype=2, size=.25) + 
      annotate("text", x=dmin, y = qnt["85%"], col = "orange", label="85%") + 
      geom_hline(yintercept =  qnt["95%"], colour = "red", linetype=2, size=.25) +
      annotate("text", x=dmin, y = qnt["95%"], col = "red", label="95%") +
      theme_bw() +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            legend.position="top", legend.key = element_blank()) + 
      scale_x_date(date_labels = "%d-%b-%y", breaks = breaks)
    
    ggplotly(p) %>% layout(dragmode = "zoom")
  })
  
  # Lead time and throughput evolution over time
  output$evolution <- renderPlot({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    time <- input$data[input$closedIdx,"LeadTime"]
    data <- input$data.all[input$closedIdx,]
    
    #stat_summary(fun.y=function(x) {quantile(x, .85)}) + 
    
    pctl85 <- function(x) {
      quantile(x, .85)
    }
    
    mean_stats <- function(x) {
      x <- stats::na.omit(x)
      x.mean <- mean(x)
      x.sd <- sd(x)
      se <- x.sd/sqrt(length(x))
      x.conf.int <- se * qnorm(0.975) 
      qnt <- quantile(x, c(.25,.5,.75))
      x.cv <- (x.sd/x.mean)*100 #coeficient of variation
      c(mean = x.mean, conf.int = x.conf.int, cv=x.cv, y = x.mean, ymin = x.mean - x.conf.int, ymax = x.mean + x.conf.int) 
    }
    
    conf.ev <- do.call(data.frame, aggregate(LeadTime ~ ClosedMonth, data, FUN = mean_stats))
    p1 <- ggplot(data,aes_string(x="ClosedMonth",y="1")) 
    legendtitle <- c()
    if (nchar(input$columnType)>0) {
      p1 <- p1 + stat_summary(aes_string(fill=input$columnType), fun.y=sum, position="stack", geom="bar")
      legendtitle <- element_text()
    } else {
      p1 <- p1 + stat_summary(aes(fill="items"), fun.y=sum, position="stack", geom="bar") 
      legendtitle <- element_blank()
    }
    p1 <- p1 + stat_summary(aes(label=..y..), fun.y=sum, geom="text", vjust = -.25) +
      theme_bw() + xlab("Closing date") + ylab("Throughput (work items/month)") + 
      theme(legend.position="bottom", legend.key = element_blank(), legend.title=legendtitle) +
      scale_fill_brewer(palette="Pastel2")
    
    p2 <- ggplot(data,aes(x=ClosedMonth, y=LeadTime)) +  
      stat_summary(aes(fill="95% conf.int"), geom="ribbon", fun.data=mean_stats, color=NA, alpha=.15, show.legend = FALSE) +
      stat_summary(aes(colour="mean", shape="mean", group=1, label=..y..), fun.y=mean, geom="line", size=0.7, linetype="dashed") +
      stat_summary(aes(colour="pctl85", shape="pctl85", group=1, label=..y..), fun.y=pctl85, geom="line", size=1.1) +
      stat_summary(aes(label= round(..y..), colour="pctl85"), fun.y=pctl85, geom="label", show.legend = FALSE, size=5) +
      geom_text(aes(x=ClosedMonth, y=LeadTime.mean, label=paste0(round(LeadTime.mean), "??", round(LeadTime.conf.int), " (", round(LeadTime.cv), "%)"), colour="mean", parse=T), data=conf.ev, vjust = 1.5, show.legend = FALSE) +
      theme_bw() + labs(colour="Lead time") +
      xlab("Closing month") + ylab("Lead time (days)") + 
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.border = element_blank(),
            legend.position="top", legend.key = element_blank())
    
    
    ggplot_dual_axis(p1, p2)
    
  })
  
  output$aging <-  renderPlotly({
    input <- dataInput()
    if (is.null(input))
      return(NULL)
    data <- input$data.all[input$openIdx,]
    nm <- names(data)[1]
    data[,"ID"] <- apply(data, 1, function(x) { paste0(nm, ": ", x[1]) }) 
    qnt <- input$qnt
    set.seed(1)
    
    data[,"lastState"] <- apply(data, 1, function(x) {
      col <- max(which(!is.na(x[input$dateCols])))
      paste0(str_pad(col, 2, pad="0"), ". ", names(data)[input$dateCols[col]])
    })
    
    data[,"random"] <- runif(dim(data)[1])
    p <- ggplot(data, aes(x = lastState, y = Age)) + 
      geom_jitter(aes(text=ID), alpha = 0.3, colour=rgb(0,.4,0), width=.5, height=0) 
    
    if (sum(is.na(qnt))==0) {
      p <- p + geom_hline(yintercept = qnt["50%"], colour = "goldenrod1", linetype=2, size=.25) + 
      annotate("text", x=0, y = qnt["50%"], colour = "goldenrod1", label="50%") +
      geom_hline(yintercept = qnt["85%"], colour = "orange", linetype=2, size=.25) + 
      annotate("text", x=0, y = qnt["85%"], col = "orange", label="85%") + 
      geom_hline(yintercept =  qnt["95%"], colour = "red", linetype=2, size=.25) +
      annotate("text", x=0, y = qnt["95%"], col = "red", label="95%")
    }
    
    p <- p + theme_bw() +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.position="top", legend.key = element_blank(),
                        axis.text.x = element_text(angle = 90),
                        axis.title.x=element_blank())
    
    ggplotly(p) %>% layout(dragmode = "zoom")
  })
  
  output$aging_desc <- renderUI({
    input <- dataInput()
    data <- input$data.all[input$openIdx,];
    
    if (is.null(input))
      return(NULL)
   
    data[,"lastState"] <- apply(data, 1, function(x) {
      col <- max(which(!is.na(x[input$dateCols])))
      paste0(str_pad(col, 2, pad="0"), ". ", names(data)[input$dateCols[col]])
    })
    
    rows <- list(
      tags$thead(
        tags$tr(
          tags$th("State"), tags$th("Mean"), tags$th("50%ile"), tags$th("85%ile"), tags$th("95%ile")
        )
      )
    )
    
    for (i in 1:(length(input$dateCols)-1)) {
      stateName <- paste0(str_pad(i, 2, pad="0"), ". ", names(data)[input$dateCols[i]])
      age <- data[which(data[,"lastState"]==stateName),"Age"]
      mean <- mean(age)
      qnt <- quantile(age, probs=c(0, .25, .50, .75, .85, .95, 1), na.rm = T)
      rows[[length(rows)+1]] <- tags$tr(
        tags$td(stateName),tags$td(round(mean)), tags$td(round(qnt["50%"])), tags$td(round(qnt["85%"])), tags$td(round(qnt["95%"]))
      )
    }
    
    age <- data[,"Age"]
    mean <- mean(age)
    qnt <- quantile(age, probs=c(0, .25, .50, .75, .85, .95, 1), na.rm = T)
    rows[[length(rows)+1]] <- tags$tr(
      tags$td("TOTAL"),tags$td(round(mean)), tags$td(round(qnt["50%"])), tags$td(round(qnt["85%"])), tags$td(round(qnt["95%"]))
    )
    
    do.call(tags$table,rows)
  })
})

ggplot_dual_axis = function(plot1, plot2, which.axis = "y") {
  
  grid::grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  if(which.axis == "x") 
    
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    
    g = gtable_add_rows(g, g2$heights[1], 1)
    
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    
  } else {
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    
  }
  
  # extract legend
  #leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
  #leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
  
  #g$grobs[[which(g$layout$name == "guide-box")]] <- 
   # gtable:::cbind_gtable(leg1, leg2, "first")
  
  # Draw it
  grid::grid.draw(g)
  
}

