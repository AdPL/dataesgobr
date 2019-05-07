library(shiny)
library(shinythemes)
library(shinyjs)

ui <- bootstrapPage(theme = shinytheme("paper"),
  useShinyjs(),
  navbarPage("DataesgobR!", id = "tabs",
             tabPanel("Main",
                      wellPanel(
                          tabsetPanel(
                            tabPanel("Search by title",
                                  textInput(inputId = "title", label = "",
                                            placeholder = "Title"),
                                  actionButton("submit", "Search"),
                                  textOutput("error"),
                                  textOutput("selected")
                           )
                         )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(
                                 textOutput("searchState"),
                                 DT::dataTableOutput("datasetsTable")
                               )
                        )
                      )
             ),
             tabPanel("Work",
                      fluidRow(
                        column(6,
                          wellPanel(
                            h5(textOutput("datasetTitleSelected")),
                            p(textOutput("datasetDescriptionSelected")),
                            p(textOutput("datasetPublisherSelected"))
                          )
                        ),
                        column(6,
                           wellPanel(
                            h5("Available Formats"),
                            DT::dataTableOutput("datasetFormatsSelected")
                           )
                        )
                      )
            )
  )
)
