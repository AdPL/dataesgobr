library(shiny)

ui <- navbarPage(
  "DataesgobR",
  tabPanel("Search"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "title", label = "Title"),
      actionButton("submit", "Search")
    ),
    mainPanel(
      fluidRow(
        column(12,
          DT::dataTableOutput('datasetsTable'))
      )
    )
  )
)
