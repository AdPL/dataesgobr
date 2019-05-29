library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(dplyr)

ui <- bootstrapPage(theme = shinytheme("paper"),
  useShinyjs(),
  navbarPage("DataesgobR!", id = "tabs",
             tabPanel("Main",
                      fluidRow(
                        column(4,
                      wellPanel(
                          textOutput("state"),
                          tabsetPanel(
                            tabPanel("Search by title",
                                  textInput(inputId = "title", label = "",
                                            placeholder = "Title"),
                                  selectInput("themeSelectInput", "Theme", {}, multiple = TRUE),
                                  selectInput("spatialSelectInput", "Spatial", {}),
                                  selectInput("publisherSelectInput", "Publisher", {}),
                                  selectInput("languageSelectInput", "Language", {c("es", "eu")}),
                                  actionButton("submit", "Search")

                           )
                         )
                      )),
                        column(8,
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
                        column(5,
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
                        column(7,
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
                            downloadButton("saveCompletedData", "Save completed data"),
                            downloadButton("saveFilteredData", "Save selected data"),
                            uiOutput("pdfViewer")
                          )
                        )
                      ),
                      fluidRow(
                        column(4,
                               wellPanel(
                                 selectInput("plotColumnSelect", "Column", {}),
                                 selectInput("plotTypeSelect", "Type of graphic",
                                             {c("plot", "hist", "pie")}),
                                 checkboxInput("plotSelectedCheck", "Use selected rows", FALSE),
                                 actionButton("loadPlot", "Load plot")
                               )
                        ),
                        column(8,
                               wellPanel(
                                 plotOutput("plot"),
                                 downloadButton("saveGeneratedPlot", "Save plot")
                               )
                        )
                      )
            )
  )
)
