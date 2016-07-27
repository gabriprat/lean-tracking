library(shiny)
library(dygraphs)

shinyUI(fluidPage(theme = "style.css",
  titlePanel(windowTitle = "Lean tracking App",
    tags$span(
    tags$a(href="http://leantracking.com", "Lean Tracking"), 
    span(" » App"))),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$small(
        paste0("The expected format is a CSV with some date columns in order (e.g. opened should precede closed). ",
        "The names of the Opened and Closed columns have to match the ones defined below. ")
      ),
      tags$hr(),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   selected=';'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   selected='"'),
      tags$hr(),
      textInput("encoding", "File encoding", value="iso-8859-15"),
      radioButtons("outliers", "Outlier exclusion", 
                   c("None" = "none",
                     "Above 95%" = "ninetyfive",
                     "Above Q3 + 1.5*IQR" = "iqr"), 
                   selected="ninetyfive"),
      textInput("columnOpened", "Opened column title", value="Open"),
      textInput("columnClosed", "Closed column title", value="Closed"),
      textInput("columnState", "State column title", value="State"),
      textInput("columnType", "Type column title", value="Type"),
      textInput("closedState", "Closed state value", value="Closed"),
      textInput("discardedState", "Discarded state value", value="Discarded"),
      textInput("dateFormat", "Date format", value="%Y-%m-%d %H:%M"),
      tags$small(
        "Here you can find the ",
        tags$a(href="https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html", "expected format masks")
      )
    ),
    mainPanel(
      tags$head(tags$script(src="script.js"), tags$script(src="jquery.sticky.js"), tags$script(src="html2canvas.js"), tags$script(src="FileSaver.min.js")),
      tags$div(class = "fixme", checked = NA,
        tabsetPanel(
          tabPanel("Data", dataTableOutput('data')),
          tabPanel("CFD", dygraphOutput("cfd", height=500), div(id="labels")),
          tabPanel("Histogram", plotOutput("histo")),
          tabPanel("Scatter plot", plotOutput("scatter")),
          tabPanel("Evolution", plotOutput("evolution"))
        )
      )
    )
  )
))
