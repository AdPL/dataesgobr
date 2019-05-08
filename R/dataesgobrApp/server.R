library(dataesgobr)
library(dplyr)

server <- function(input, output, session) {
  generateButton <- function(FUN, len, id, ...) {
    buttons <- character(len)
    for (i in seq_len(len)) {
      buttons[i] <- as.character(FUN(paste0(id, i), ...))
    }
    buttons
  }

  observeEvent(input$submit, {
    datasets <- search_by_title(input$title)
    if (length(datasets) == 0) {
      output$searchState <- renderText("0 matches found.")
      output$datasetsTable <- DT::renderDT({})
    } else {
      output$searchState <- renderText("")
      output$datasetsTable <- DT::renderDataTable({
        data <- do.call(rbind,
                        Map(data.frame,
                            ID = rownames(datasets),
                            Title = as.character(datasets$title),
                            Description = as.character(datasets$description),
                            About = paste0("<a href='", datasets$`_about`,
                                           "' target='_blank'>Open in datos.gob.es</a>"),
                            Actions = generateButton(actionButton,
                                                     nrow(datasets),
                                                     'button_',
                                                     label = "Load dataset",
                                                     onclick = 'Shiny.onInputChange(\"select_button\", this.id)'),
                            Url = datasets$`_about`))
        datasets_downloaded <<- data
        return(data[2:5])
      }, escape = FALSE, selection = "none")
      addClass("datasetsTable", "table-responsive")
    }
  })

  observeEvent(input$select_button, {
    datasetSelected <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    data_preload <<- search_by_id(dataesgobr:::get_id(datasets_downloaded[datasetSelected,]$Url))

    showNotification(paste("Dataset loaded"), type = "message", duration = 4)

    output$datasetTitleSelected <- renderText(data_preload$title)
    output$datasetDescriptionSelected <- renderText(data_preload$description)
    output$datasetPublisherSelected <- renderText(paste("Publisher: ", data_preload$publisher))

    if (is.null(data_preload$formats_info)) {
      data_preload$formats_info <- "No info"
    }

    output$datasetFormatsSelected <- DT::renderDataTable({
      formats <- do.call(rbind,
                         Map(data.frame,
                             Format = names(data_preload$formats),
                             Url = paste0("<a href='", data_preload$formats,
                                          "' target='_blank'>Descargar</a>"),
                             Actions = generateButton(actionButton,
                                                      length(data_preload$formats),
                                                      'button_',
                                                      label = "Load data",
                                                      onclick = 'Shiny.onInputChange(\"load_data\", this.id)'),
                             Information = data_preload$formats_info))
      return(formats)
    }, escape = FALSE, selection = "none")

    addClass("datasetFormatsSelected", "table-responsive")
    updateTabsetPanel(session, "tabs", "Work")
  })

  observeEvent(input$load_data, {
    dataSelected <- as.numeric(strsplit(input$load_data, "_")[[1]][2])
    output$dataSelected <- renderText(paste("url a cargar:", as.character(data_preload$formats[dataSelected][1])))

    showNotification(paste("Loading data, please wait..."), type = "warning", duration = 4)

    content <<- load_data(data_preload)
    output$dataTable <- DT::renderDataTable(content)

    addClass("dataTable", "table-responsive")
  })
}
