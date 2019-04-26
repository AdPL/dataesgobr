library(dataesgobr)
library(dplyr)

server <- function(input, output) {
  observeEvent(input$submit, {
    datasets <- search_by_title(input$title)

    output$datasetsTable <- renderDataTable({
      data <- do.call(rbind,
                      Map(data.frame,
                          Title = datasets$title,
                          Description = datasets$description,
                          About = paste0("<a href='", datasets$`_about`, "' target='_blank'>Open</a>")))
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
