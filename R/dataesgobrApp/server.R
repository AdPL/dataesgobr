library(dataesgobr)
library(dplyr)

server <- function(input, output, session) {
  observeEvent(input$datasetsTable_rows_selected, {
    data <- datasets_downloaded

    row <- input$datasetsTable_rows_selected
    url <- as.character(dataesgobr:::get_id(data[row,]$Url))
    dataSelected <- search_by_id(url)

    if (is.null(dataSelected$formats_info)) {
      dataSelected$formats_info <- "No info"
    }

    output$titleSelected <- renderText(dataSelected$title)
    output$formatsSelected <- renderDataTable({
      formats <- do.call(rbind,
                         Map(data.frame,
                             Format = names(dataSelected$formats),
                             Url = paste0("<a href='", dataSelected$formats,
                                          "' target='_blank'>Descargar</a>"),
                             Information = dataSelected$formats_info))
      return(formats)
    }, escape = FALSE)
    return(data)
  })

  observeEvent(input$submit, {
    datasets <- search_by_title(input$title)
    if (length(datasets) == 0) {
      output$searchState <- renderText("0 matches found.")
      output$datasetsTable <- DT::renderDT({})
    } else {
      output$searchState <- renderText("")
      output$datasetsTable <- DT::renderDT({

        data <- do.call(rbind,
                        Map(data.frame,
                            Title = datasets$title,
                            Description = datasets$description,
                            About = paste0("<a href='", datasets$`_about`,
                                           "' target='_blank'>Open</a>"),
                            Url = datasets$`_about`))
        datasets_downloaded <<- data
        return(data[1:4])
      }, escape = FALSE, selection = "single")
    }
  })

  observeEvent(input$submitId, {
    data_preload <<- data <- search_by_id(input$url)
    output$datasetTitle <- renderText(data$title)
    output$datasetUrl <- renderText(data$url)
    output$datasetPublisher <- renderText(data$publisher)
    output$datasetDescription <- renderText(data$description[[1]])

    output$datasetFormats <- renderDataTable({
      formats <- do.call(rbind,
                         Map(data.frame,
                             Format = names(data$formats),
                             Url = paste0("<a href='", data$formats, "'>Descargar</a>"),
                             Information = data$formats_info))
      return(formats)
    }, escape = FALSE)
    output$datasetIssued <- renderText(data$issued)
  })

  observeEvent(input$sendToWork, {
    output$datasetLoadTitle <- renderText(data_preload$title)
    output$datasetLoadFormats <- renderDataTable({
      formats <- do.call(rbind,
                         Map(data.frame,
                             Format = names(data_preload$formats),
                             Url = paste0("<a href='", data_preload$formats, "'>Load data</a>"),
                             Information = data_preload$formats_info))
      return(formats)
    }, escape = FALSE)
  })

  output$loadCSV <- downloadHandler(
    content <<- load_data(data_preload),
    output$dataTable <- renderDataTable(content)
  )
}
