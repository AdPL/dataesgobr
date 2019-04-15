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
}
