library(shiny)

ui <- fluidPage(
  navbarPage("DataesgobR!",
  tabPanel("Search",
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "title", label = "Title"),
                actionButton("submit", "Search"),
                hr(),
                h4(textOutput("titleSelected")),
                dataTableOutput("formatsSelected"),
                textOutput("error")
      ),
      mainPanel(
        fluidRow(
          column(12,
                 DT::dataTableOutput("datasetsTable")
          )
        )
      )
    )
  ),
  tabPanel("Load by ID",
   fluidRow(
     column(3,
      wellPanel(
        textInput(inputId = "url", label = "ID"),
        actionButton("submitId", "Load")
      )
     ),
     column(9,
      wellPanel(
        h1(textOutput("datasetTitle")),
        h5(textOutput("datasetUrl")),
        strong("Publisher:"),
        span(textOutput("datasetPublisher")),
        hr(),
        h3("Description"),
        textOutput("datasetDescription"),
        h3("Distribution"),
        dataTableOutput("datasetFormats"),
        h3("Additional info"),
        textOutput("datasetIssued")
      )
     )
    )
   )
  )
)
