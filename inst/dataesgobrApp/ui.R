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
                                  selectInput("publisherSelectInput", "Publisher", {}),
                                  selectInput("languageSelectInput", "Language", {c("es", "eu")}),
                                  actionButton("submit", "Search")

                           )
                         )
                      ),
                      wellPanel(
                        h4("How to use dataesgobr GUI"),
                        span("Version: 1.0.0"),
                        p("This web application is a tool inside the dataesgob package
                          that allow you to download, analyze and generate information
                          through datasets obtained from the API"),
                        p("Actually you can find 2 tabs: main and work"),
                        tags$ul(
                          tags$li("Main: this view allow you to send request to
                             Goversment's API in order to obtain datasets from it."),
                          tags$li("Work: when you click in any 'Load dataset' button,
                                  this dataset will be load in Work view where you
                                  can check more information, download, load,
                                  and save data associates to the dataset and draw plots.")
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
                                 h4("Plot control"),
                                 actionButton("helpButton", "How to use plot control"),
                                 checkboxInput("plotSelectedCheck", "Use selected rows", FALSE),
                                 selectInput("plotTypeSelect", "Type of graphic",
                                             {c("plot", "hist", "pie", "boxplot")}),
                                 selectInput("plotColumnSelect", "Column", {}),
                                 selectInput("plotColumn2Select", "Boxplot column (just works with boxplot type)", {}),
                                 numericInput("plotNumSelect", "Number of classes in hist (just works with hist type)", value = 2),
                                 sliderInput("plotXlimSelect", "x range", min = 0, max = 1000, value = 0),
                                 sliderInput("plotYlimSelect", "y range", min = 0, max = 1000, value = 0),
                                 actionButton("loadPlot", "Load plot")
                               )
                        ),
                        column(8,
                               wellPanel(
                                 h4("Plot result"),
                                 plotOutput("plot"),
                                 downloadButton("saveGeneratedPlot", "Save plot")
                               )
                        )
                      )
            )
  )
)
