library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(dplyr)

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
                      ),
                      fluidRow(
                        column(12,
                          wellPanel(
                            textOutput("dataSelected"),
                            DT::dataTableOutput("dataTable") %>%
                              withSpinner(color = "#0dc5c1")
                          )
                        )
                      )
            )
  )
)
