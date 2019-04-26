library(dplyr)

server <- function(input, output) {
  observeEvent(input$submit, {
    response <- jsonlite::fromJSON(paste("https://datos.gob.es/apidata/catalog/dataset/title/", input$title ,"?_sort=title&_pageSize=50&_page=0", sep = ""))
    datasets <- response[["result"]][["items"]]

    output$datasetsTable <- DT::renderDataTable(DT::datatable({
      data <- datasets %>% select("title", "description", "_about")
      data
    }))
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
