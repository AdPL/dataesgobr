library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)

ui <- bootstrapPage(theme = shinytheme("paper"),
  useShinyjs(),
  navbarPage("DataesgobR!", id = "tabs",
             tabPanel("Main",
                      wellPanel(
                          textOutput("state"),
                          tabsetPanel(
                            tabPanel("Search by title",
                                  textInput(inputId = "title", label = "",
                                            placeholder = "Title"),
                                  actionButton("submit", "Search")

                           )
                         )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(
                                 textOutput("searchState"),
                                 DT::dataTableOutput("datasetsTable")
                                  %>% withSpinner(color = "#0dc5c1")
                               )
                        )
                      )
             ),
             tabPanel("Work",
                      fluidRow(
                        column(6,
                          wellPanel(
                            h4(textOutput("datasetTitleSelected")),
                            span(htmlOutput("datasetUrlSelected")),
                            h5("Description"),
                            p(textOutput("datasetDescriptionSelected")),
                            p(textOutput("datasetPublisherSelected")),
                            h5("Additional info"),
                            p(textOutput("datasetIssuedSelected")),
                            p(textOutput("datasetKeywordsSelected"))
                          )
                        ),
                        column(6,
                           wellPanel(
                            h5("Available Formats"),
                            DT::dataTableOutput("datasetFormatsSelected")
                           )
                        )
                      ),
                      fluidRow(
                        column(12,
                          wellPanel(
                            textOutput("dataSelected"),
                            DT::dataTableOutput("dataTable") %>%
                              withSpinner(color = "#0dc5c1"),
                              uiOutput("pdfViewer")
                          )
                        )
                      )
            )
  )
)
