library(dataesgobr)
library(dplyr)

server <- function(input, output, session) {
  observeEvent(input$datasetsTable_rows_selected, {
    if (!exists("datasets_preload")) {
      data <- datasets_downloaded
    } else {
      data <- datasets_preload
    }
    row <- input$datasetsTable_rows_selected
    output$estado <- renderText(row)
    output$cargado <- renderDataTable({
      data <- do.call(rbind,
                      Map(data.frame,
                          Title = datasets_downloaded[row,]["Title"],
                          About = datasets_downloaded[row,]["About"]))
      }, escape = FALSE)
    return(data)
  })

  observeEvent(input$submit, {
    output$datasetsTable <- DT::renderDT({
      datasets <- search_by_title(input$title)

      data <- do.call(rbind,
                      Map(data.frame,
                          Title = datasets$title,
                          Description = datasets$description,
                          About = paste0("<a href='", datasets$`_about`, "' target='_blank'>Open</a>")))
      datasets_downloaded <<- data
      return(data)
    }, escape = FALSE)
  })

  observeEvent(input$submitId, {
    data <- search_by_id(input$url)
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
}
