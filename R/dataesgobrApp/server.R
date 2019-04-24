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
    formats <- do.call(rbind,
                       Map(data.frame,
                           Format = names(data$formats),
                           Url = data$formats,
                           Information = data$formats_info))
    output$datasetFormats <- renderTable(formats)
    output$datasetIssued <- renderText(data$issued)
  })
}
