library(dataesgobr)
library(dplyr)
library(DT)

server <- function(input, output, session) {
  output$datasetsTable <- DT::renderDataTable({})
  addClass("saveCompletedData", "hidden")
  addClass("saveFilteredData", "hidden")
  addClass("loadPlot", "hidden")

  e <- new.env()
  if(!exists("list_themes")) {
    e$list_themes <- get_themes_from_api()$notation
  }
  if(!exists("list_publishers")) {
    e$list_publishers <- get_publishers_from_api()
  }

  updateSelectInput(session, "themeSelectInput",
                    label = "Theme",
                    choices = e$list_themes,
                    selected = tail(e$list_themes,0))
  updateSelectInput(session, "publisherSelectInput",
                    label = "Publisher",
                    choices = e$list_publishers$prefLabel,
                    selected = tail(e$list_publishers$prefLabel,0))

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
      graphic_data(type, e$content, columns = column, dataSelected = s,
                   xlim = xrange, ylim = yrange, nClasses = nhist)
    } else {
      if (type == "boxplot" && column2 != "") {
        graphic_data(type, e$content, columns = c(column, column2),
                     xlim = xrange, ylim = yrange, nClasses = nhist)
      } else {
        graphic_data(type, e$content, columns = column,
                     xlim = xrange, ylim = yrange, nClasses = nhist)
      }
    }
    dev.off()
  }

  observeEvent(input$submit, {
    datasets <- data.frame()

    publisherSelected <- input$publisherSelectInput
    publisher <- e$list_publishers %>% filter(e$list_publishers$prefLabel == publisherSelected)

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
        e$datasets_downloaded <- data
        names(data)[names(data) == "Description._value"] <- "Description"
        return(data[,c("Title", "Description", "About", "Actions")])
      }, escape = FALSE, selection = "none")
      addClass("datasetsTable", "table-responsive")
    }
  })

  observeEvent(input$helpButton, {
    showModal(modalDialog(
      title = "How to use plot control",
      p("Plot control is a panel to adjust and generate graphics. It is composed of
      7 different controls:"),
      p("- Use selected rows: if this checkbox is active then the plot generated just
        contains the rows that you selected in the table above."),
      p("- Type of graphic: in this version you can select between four type of
        graphics: plot, hist, pie or boxplot."),
      p("- Column: when you load data in the table above this selectbox will be
        updated containing the columns in the dataset. So you can select the column
        that you want to represent."),
      p("- Boxplot column: if you want to use boxplot representation you will need
        to use two columns, then you must complete this selectbox. So select the
        'no-number column' in this selectbox, the number column must be selected
        in the previous selectbox (column)."),
      p("- Number of classes: this number input indicates the maximum number of
        classes in case that you select hist on the type selectbox."),
      p("- X range and Y range slider: the sliders have the objetive of adjust the
        graphic in order to produce nice graphics."),
      footer = modalButton("Ok")
    ))
  })

  observeEvent(input$select_button, {
    datasetSelected <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    e$data_preload <- search_by_id(dataesgobr:::get_id(e$datasets_downloaded[datasetSelected,]$Url))

    showNotification(paste("Dataset loaded"), type = "message", duration = 4)

    output$datasetTitleSelected <- renderText(e$data_preload$title)
    output$datasetUrlSelected <- renderUI(tagList(a("Look in datos.gob.es", href = e$data_preload$url, target = "_blank")))
    output$datasetDescriptionSelected <- renderText(e$data_preload$description)
    output$datasetPublisherSelected <- renderText(paste("Publisher:", get_publisher(get_id(e$data_preload$publisher))$prefLabel))
    output$datasetIssuedSelected <- renderText(paste("Issued: ", e$data_preload$issued))
    output$datasetKeywordsSelected <- renderText(unlist(e$data_preload$keywords))

    if (is.null(e$data_preload$formats_info)) {
      data_preload$formats_info <- "No info"
    }

    output$datasetFormatsSelected <- DT::renderDataTable({
      formats <- do.call(rbind,
                         Map(data.frame,
                             Format = names(e$data_preload$formats),
                             Url = paste0("<a href='", e$data_preload$formats,
                                          "' target='_blank'>Descargar</a>"),
                             Actions = generateButton(actionButton,
                                                      length(e$data_preload$formats),
                                                      'button_',
                                                      label = "Load data",
                                                      onclick = 'Shiny.onInputChange(\"load_data\", this.id)'),
                             Information = e$data_preload$formats_info))
      return(formats[2:4])
    }, escape = FALSE, selection = "none", options = list(pageLength = 10))

    addClass("datasetFormatsSelected", "table-responsive")
    updateTabsetPanel(session, "tabs", "Work")
  })

  observeEvent(input$load_data, {
    dataSelected <- as.numeric(strsplit(input$load_data, "_")[[1]][2])
    fileSelected <- as.character(e$data_preload$formats[dataSelected][1])

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
      download_data(e$data_preload, format, FALSE, dataSelected, noconfirm = TRUE)
      e$content <- load_data(fileSelected)

      elementColumns <- names(e$content)
      elementColumns <- append(elementColumns, " ", length(elementColumns))
      updateSelectInput(session, "plotColumnSelect",
                        label = "Column",
                        choices = elementColumns,
                        selected = tail(elementColumns, 0))

      updateSelectInput(session, "plotColumn2Select",
                        choices = elementColumns,
                        selected = tail(elementColumns, 0))

      updateSliderInput(session, "plotXlimSelect",
                        label = "x range",
                        value = 0,
                        min = 0,
                        max = (nrow(e$content))/ncol(e$content),
                        step = 1)

      updateSliderInput(session, "plotYlimSelect",
                        label = "y range",
                        value = 0,
                        min = 0,
                        max = nrow(e$content),
                        step = 1)

      output$dataTable <- DT::renderDataTable(e$content, editable = TRUE, filter = "top")

      addClass("dataTable", "table-responsive")
      removeClass("saveCompletedData", "hidden")
      removeClass("saveFilteredData", "hidden")
      removeClass("loadPlot", "hidden")
    }
  })

  output$saveCompletedData <- downloadHandler("content_complete.csv",
    content = function(file) {
     write.csv(e$content, file)
    }
  )

  output$saveFilteredData <- downloadHandler("content_filtered.csv",
    content = function(file) {
      s <- input$dataTable_rows_selected
      if (length(s) > 0) {
        write.csv(e$content[s, , drop = FALSE], file)
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
      plot <- generateGraph(file)
      print(plot)
    },
    contentType = "image/png"
  )
}
