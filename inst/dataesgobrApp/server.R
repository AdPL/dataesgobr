library(dataesgobr)
library(dplyr)
library(DT)

server <- function(input, output, session) {
  output$datasetsTable <- DT::renderDataTable({})

  if(!exists("list_themes")) {
    list_themes <<- get_themes_from_api()$notation
  }
  if(!exists("list_spatials")) {
    list_spatials <<- get_spatials_from_api()
  }
  if(!exists("list_publishers")) {
    list_publishers <<- get_publishers_from_api()
  }

  updateSelectInput(session, "themeSelectInput",
                    label = "Theme",
                    choices = list_themes,
                    selected = tail(list_themes,0))
  updateSelectInput(session, "spatialSelectInput",
                    label = "Spatial",
                    choices = list_spatials$label,
                    selected = tail(list_spatials$label,0))
  updateSelectInput(session, "publisherSelectInput",
                    label = "Publisher",
                    choices = list_publishers$prefLabel,
                    selected = tail(list_publishers$prefLabel,0))

  generateButton <- function(FUN, len, id, ...) {
    buttons <- character(len)
    for (i in seq_len(len)) {
      buttons[i] <- as.character(FUN(paste0(id, i), ...))
    }
    buttons
  }

  generateGraph <- function(outfile) {
    column <- input$plotColumnSelect
    column2 <- input$plotColumn2Select
    type <- input$plotTypeSelect
    nhist <- input$plotNumSelect
    check <- input$plotSelectedCheck
    xrange <- input$plotXlimSelect
    yrange <- input$plotYlimSelect
    width  <- session$clientData$output_plot_width
    height <- session$clientData$output_plot_height
    pixelratio <- session$clientData$pixelratio

    png(outfile, width = width*pixelratio, height = height*pixelratio,
        res = 72*pixelratio)
    if (check) {
      s <- input$dataTable_rows_selected
      graphic_data(type, content, columns = column, dataSelected = s,
                   xlim = xrange, ylim = yrange, nClasses = nhist)
    } else {
      if (type == "boxplot" && column2 != "") {
        graphic_data(type, content, columns = c(column, column2),
                     xlim = xrange, ylim = yrange, nClasses = nhist)
      } else {
        graphic_data(type, content, columns = column,
                     xlim = xrange, ylim = yrange, nClasses = nhist)
      }
    }
    dev.off()
  }

  observeEvent(input$submit, {
    datasets <- data.frame()

    spatialSelected <- input$spatialSelectInput
    message(spatialSelected)

    publisherSelected <- input$publisherSelectInput
    publisher <- list_publishers %>% filter(list_publishers$prefLabel == publisherSelected)

    themesSelected <- input$themeSelectInput

    language <- input$languageSelectInput

    datasets <- dataesgobr:::search_by(input$title, themesSelected, publisher = publisher$notation)

    if (length(datasets) == 0) {
      output$searchState <- renderText("0 matches found.")
      output$datasetsTable <- DT::renderDT({})
    } else {
      output$searchState <- renderText("")
      data <- do.call(rbind,
                      Map(data.frame,
                          ID = rownames(datasets),
                          Title = as.character(datasets$title),
                          Description = datasets$description,
                          About = paste0("<a href='", datasets$`_about`,
                                         "' target='_blank'>Open in datos.gob.es</a>"),
                          Actions = generateButton(actionButton,
                                                   nrow(datasets),
                                                   'button_',
                                                   label = "Load dataset",
                                                   onclick = 'Shiny.onInputChange(\"select_button\", this.id)'),
                          Url = datasets$`_about`))

      data <- data %>% filter(data$`Description._lang` == language)

      output$datasetsTable <- DT::renderDataTable({data
        datasets_downloaded <<- data
        names(data)[names(data) == "Description._value"] <- "Description"
        return(data[,c("Title", "Description", "About", "Actions")])
      }, escape = FALSE, selection = "none")
      addClass("datasetsTable", "table-responsive")
    }
  })

  observeEvent(input$select_button, {
    datasetSelected <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    data_preload <<- search_by_id(dataesgobr:::get_id(datasets_downloaded[datasetSelected,]$Url))

    showNotification(paste("Dataset loaded"), type = "message", duration = 4)

    output$datasetTitleSelected <- renderText(data_preload$title)
    output$datasetUrlSelected <- renderUI(tagList(a("Look in datos.gob.es", href = data_preload$url, target = "_blank")))
    output$datasetDescriptionSelected <- renderText(data_preload$description)
    output$datasetPublisherSelected <- renderText(paste("Publisher: ", get_publisher(get_id(data_preload$publisher))))
    output$datasetIssuedSelected <- renderText(paste("Issued: ", data_preload$issued))
    output$datasetKeywordsSelected <- renderText(unlist(data_preload$keywords))
    output$dataTable <- DT::renderDataTable({})

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
      return(formats[2:4])
    }, escape = FALSE, selection = "none", options = list(pageLength = 5))

    addClass("datasetFormatsSelected", "table-responsive")
    updateTabsetPanel(session, "tabs", "Work")
  })

  observeEvent(input$load_data, {
    dataSelected <- as.numeric(strsplit(input$load_data, "_")[[1]][2])
    fileSelected <- as.character(data_preload$formats[dataSelected][1])

    showNotification(paste("Loading data, please wait..."), type = "warning", duration = 4)

    output$dataTable <- DT::renderDataTable({})
    format <- dataesgobr:::get_format(fileSelected)
    if (length(format) == 0) {
      showNotification(paste("Error loading the file"), type = "error", duration = 4)
      showModal(modalDialog(
        title = "Error!",
        paste0("Error loading selected dataset, you can try to download and load by your own"),
        footer = modalButton("Ok")
      ))
    } else {
      download_data(data_preload, format, FALSE, dataSelected)
      content <<- load_data(fileSelected)

      updateSelectInput(session, "plotColumnSelect",
                        label = "Column",
                        choices = names(content),
                        selected = tail(names(content), 0))

      updateSelectInput(session, "plotColumn2Select",
                        choices = names(content),
                        selected = tail(names(content), 0))

      updateSliderInput(session, "plotYlimSelect",
                        label = "y range",
                        value = 0,
                        min = 0,
                        max = nrow(content),
                        step = 1)

      output$dataTable <- DT::renderDataTable(content, editable = TRUE, filter = "top")

      addClass("dataTable", "table-responsive")
    }
  })

  output$saveCompletedData <- downloadHandler("content_complete.csv",
    content = function(file) {
     write.csv(content, file)
    }
  )

  output$saveFilteredData <- downloadHandler("content_filtered.csv",
    content = function(file) {
      s <- input$dataTable_rows_selected
      if (length(s) > 0) {
        write.csv(content[s, , drop = FALSE], file)
      } else {
        showModal(modalDialog(
          title = "Error!", "You must select one row at least",
          footer = modalButton("Ok")
        ))
      }
    }
  )

  observeEvent(input$loadPlot, {
    output$plot <- renderImage({
      outfile <- tempfile(fileext='.png')
      generateGraph(outfile)
      list(src = outfile,
           alt = "This is alternate text")
    }, deleteFile = TRUE)

  })

  output$saveGeneratedPlot <- downloadHandler(
    filename <- function() {
      paste("plot", ".png", sep = "")
    },
    content <- function(file) {
      generateGraph(file)
      print(plot)
      dev.off()
    },
    contentType = "image/png"
  )
}
