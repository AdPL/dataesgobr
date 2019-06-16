#' @title Creates a data.frame containing datasets from datos.gob.es
#' @description Send a request to datos.gob.es using the title param
#' to search datasets that match with the title, then the results are returned
#' as data.frame
#'
#' @param title Title to search
#' @param numentry Number of results for page
#' @param page The number of page to see, the first page is 0
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' # Return first 50 matches that contain puente in their title
#' mydataesgobr <- search_by_title('puente')
#'
#' # Return the first 78 matches that contain gasto in their title
#' mydataesgobr <- search_by_title('gasto', 78)
#'
#' # Return the first 78 matches that contain gasto in their title found in the
#' second page (number 1)
#' mydataesgobr <- search_by_title('gasto', 78, page = 1)
#' }
#' @export
#' @return A data.frame containing information about datasets that match with
#' the title param
search_by_title <- function(title, numentry = 50, page = 0) {
  stopifnot(is.character(title), is.numeric(numentry), is.numeric(page))
  data <- data.frame()

  search <- make_url("title", title, c("pagesize" = numentry, "page" = page))
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title Creates an object of the class dataesgobr that matches with the
#' dataset id
#'
#' @param id A string
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' dataset <- search_by_id('l01280066-presupuestos-20141')
#' dataset <- search_by_id('https://datos.gob.es/es/catalogo/l01280066-tramites-salud1')
#' }
#' @export
#' @return A dataesgobr object
search_by_id <- function(id) {
  stopifnot(is.character(id))

  id <- get_id(id)
  search <- make_url("id", id)
  response <- jsonlite::fromJSON(search)

  datos <- response[["result"]][["items"]]

  if (length(datos) == 0) {
    datos <- NULL
  } else {
    dataesgobr_from_json(json = datos)
  }
}

#' @title Creates a data.frame containing datasets from datos.gob.es
#' @description Send a request to datos.gob.es using the theme param
#' to search datasets that match with the theme, then the results are returned
#' as data.frame
#'
#' @param theme Theme to search
#' @param numentry Number of results for page
#' @param page The number of page to see, the first page is 0
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasetsPublic <- search_by_theme('sector-publico', 20)
#' datasetsSalud <- search_by_theme('salud', page = 2)
#' }
#' @export
#' @return A data.frame
search_by_theme <- function(theme, numentry = 50, page = 0) {
  stopifnot(is.character(theme), is.numeric(numentry), is.numeric(page))
  data <- data.frame()

  search <- make_url("theme", theme, c("pagesize" = numentry, "page" = page))
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title Creates a data.frame containing datasets from datos.gob.es
#' @description Send a request to datos.gob.es using spatial params
#' to search datasets that match with the spatial1 and spatial2, then
#' the results are returned as data.frame
#'
#' @param spatial1 First word of spatial. It can be: "Autonomia", "Pais" or "Provincia"
#' @param spatial2 Second word of spatial
#' @param numentry Number of results for page
#' @param page The number of page to see, the first page is 0
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasetsAndalucia <- search_by_spatial('Autonomia', 'Andalucia')
#' datasetsJaen <- search_by_spatial('Provincia', 'Jaen')
#' }
#' @export
#' @return A data.frame
search_by_spatial <- function(spatial1, spatial2, numentry = 50, page = 0) {
  stopifnot(is.character(spatial1), is.character(spatial1), is.numeric(numentry),
            is.numeric(page))
  data <- data.frame()

  spatial1 <- chartr('áéíóú', 'aeiou', spatial1)
  spatial2 <- chartr('áéíóú', 'aeiou', spatial2)

  search <- make_url("spatial", paste0(spatial1, "/", spatial2),
                      c("pagesize" = numentry, "page" = page))
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title Creates a data.frame containing datasets from datos.gob.es
#' @description Send a request to datos.gob.es using the publisher param
#' to search datasets that match with the publisher, then the results are returned
#' as data.frame
#'
#' @param publisher Publisher to search
#' @param numentry Number of results for page
#' @param page The number of page to see, the first page is 0
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' url <- make_url("publisher", 'L01280066')
#' publisherID <- get_id(url)
#' datasets <- search_by_publisher(publisherID)
#' datasets <- search_by_publisher('L01280066')
#' }
#' @export
#' @return A data.frame
search_by_publisher <- function(publisher, numentry = 50, page = 0) {
  stopifnot(is.character(publisher), is.numeric(numentry), is.numeric(page))
  data <- data.frame()

  search <- make_url("publisher", publisher, c("pagesize" = numentry, "page" = page))
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title Creates a data.frame containing datasets from datos.gob.es that matches
#' the parameters passed
#'
#' @param title Title field
#' @param theme Theme field
#' @param publisher Publisher field
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by('atestados', 'salud', 'L01280066')
#' }
#' @export
#' @return A data.frame
search_by <- function(title, theme, publisher) {
  datasets <- data.frame()

  if (!missing(title) && !is.null(title) && title != "") {
    stopifnot(is.character(title))
    title_data <- search_by_title(title, 200, 0)
    datasets <- title_data
  }

  if (!missing(theme) && !is.null(theme)) {
    stopifnot(is.character(theme))
    for (item in theme) {
      theme_data <- search_by_theme(item, 200, 0)
      if (length(datasets) == 0)
        datasets <- theme_data
      else
        datasets <- merge(datasets, theme_data, all = TRUE)
    }
  }

  if (!missing(publisher) && length(publisher) != 0) {
    stopifnot(is.character(publisher))
    publisher_data <- search_by_publisher(publisher, 200, 0)
    if (length(datasets) == 0)
      datasets <- publisher_data
    else
      datasets <- merge(datasets, publisher_data, all = TRUE)
  }

  datasets
}

#' @title Filter data.frame by title using q param
#'
#' @param data A data.frame that will be filtered
#' @param q A string to match in data.frame data
#' @param quiet Set if the function can print info messages or not
#' @import dplyr
#' @import stringr
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('salud')
#' datasetsFiltered <- filter_by_title(datasets, 'encuesta')
#' }
#' @export
#' @return A data.frame with rows that matches the dataset title
filter_by_title <- function(data, q, quiet = FALSE) {
  stopifnot(class(data) == 'data.frame', class(q) == 'character',
            is.logical(quiet))

  result <- filter(data, str_detect(
    str_to_lower(unlist(as.character(data$title))),
    str_to_lower(q)))

  nMatches <- nrow(result)
  if (!quiet) {
    message("Found ", nMatches, " matches.")
  }
  result
}

#' @title Filter data.frame by description using q param
#'
#' @param data A data.frame that will be filtered
#' @param q A string to match in data.frame data
#' @param quiet Set if the function can print info messages or not
#' @import dplyr
#' @import stringr
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('salud')
#' datasetsFiltered <- filter_by_description(datasets, 'salud')
#' }
#' @export
#' @return A data.frame with rows that matches the description
filter_by_description <- function(data, q, quiet = FALSE) {
  stopifnot(class(data) == 'data.frame', class(q) == 'character',
            is.logical(quiet))

  result <- filter(data, str_detect(
    str_to_lower(unlist(as.character(data$description))),
    str_to_lower(q)))

  nMatches <- nrow(result)
  if (!quiet) {
    message("Found ", nMatches, " matches.")
  }
  result
}

#' @title Filter data.frame by keywords using keywords param
#'
#' @param data A data.frame that will be filtered
#' @param keywords A string to match in data.frame data
#' @param quiet Set if the function can print info messages or not
#' @import dplyr
#' @import stringr
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('salud')
#' datasetsFiltered <- filter_by_keywords(datasets, 'enfermedad')
#' }
#' @export
#' @return A data.frame that matches any given keyword
filter_by_keywords <- function(data, keywords, quiet = FALSE) {
  stopifnot(class(data) == 'data.frame', class(keywords) == 'character',
            is.logical(quiet))

  result <- filter(data, str_detect(
    str_to_lower(unlist(as.character(data$keyword))),
    str_to_lower(keywords)))

  nMatches <- nrow(result)
  if (!quiet) {
    message("Found ", nMatches, " matches.")
  }
  result
}

#' @title Filter data.frame using params passed
#'
#' @param data A data.frame that will be filtered
#' @param title A string to match with the title column in the data.frame
#' @param description A string to match with the description column in the data.frame
#' @param keywords A string to match with the keywords column in the data.frame
#' @param quiet A logical param that set if the function will print info message
#' in console
#' @import dplyr
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('salud')
#' datasetsFiltered <- filter_by(datasets, 'salud', 'vacuna')
#' datasetsFiltered2 <- filter_by(datasets, 'salud', keywords = 'enfermedad')
#' }
#' @export
#' @return A data.frame that matches
filter_by <- function(data, title = NULL, description = NULL, keywords = NULL,
                      quiet = FALSE) {
  stopifnot(is.data.frame(data))

  if (!missing(title)) {
    stopifnot(is.character(title))
    data %>% filter_by_title(title, quiet = TRUE) -> data
  }
  if (!missing(description)) {
    stopifnot(is.character(description))
    data %>% filter_by_description(description, quiet = TRUE) -> data
  }
  if (!missing(keywords)) {
    stopifnot(is.character(keywords))
    data %>% filter_by_keywords(keywords, quiet = TRUE) -> data
  }

  if (!quiet) {
    nMatches <- nrow(data)
    message("Found ", nMatches, " matches.")
  }
  data
}

#' @title Represent keywords in a plot
#'
#' @param list List with keywords
#' @param type Visual representation of the plot
#' @param nrepeats Minimun number of ocurrences to draw in the plot
#'
#' @export
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('salud')
#' keywords <- get_all_keywords(datasets)
#' graphic_keywords(keywords, 'circular', 10)
#'
#' datasets <- search_by_title('salud')
#' keywords <- get_all_keywords(datasets)
#' graphic_keywords(keywords, 'barras', 11)
#' }
#' @import graphics
#' @import grDevices
#' @import utils
graphic_keywords <- function(list, type = "circular", nrepeats = 0) {
  stopifnot(is.list(list), is.character(type), is.numeric(nrepeats))
  list <- list[list>nrepeats]
  switch (type,
    circular = {
       pie(sapply(list, unlist),
          labels = paste(names(list), ": ", unlist(list), sep = ""),
          col = rainbow(7), radius = 1.0)
      },
     barras = {
       barplot(sapply(list, unlist),
               col = rainbow(7),
               xlab = "Keywords",
               ylab = "count")
     }
  )
  title("Ocurrencias de keywords")
}

#' @title Obtain keywords in data.frame with the number of ocurrences
#'
#' @param data A data.frame with datasets from datos.gob.es
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('consumo')
#' keywords <- get_all_keywords(datasets)
#' }
#' @export
#' @return A list with keywords
get_all_keywords <- function(data) {
  stopifnot(class(data) == 'data.frame')
  list_of_keywords <- list()
  keywords <- list()

  for (row in data$keyword) {
    for (keyword in row) {
      if(is.element(keyword, names(list_of_keywords))) {
        list_of_keywords[[keyword]] <- list_of_keywords[[keyword]] + 1
      } else {
        list_of_keywords[[keyword]] <- 1
      }
    }
  }
  list_of_keywords
}

#' @title Load dataset from a data.frame
#'
#' @param dataframe A data.frame with datasets from datos.gob.es
#' @param row A number of the row in the data.frame. If the dataset has more
#' than one element this param determinates the row to load. If row parameter is
#' missing then the function will load the first row
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('salud')
#' dataset <- load_dataset(datasets)
#'
#' datasets <- search_by_title('consumo')
#' dataset <- load_dataset(datasets, 5)
#' }
#' @export
#' @return A dataesgobr object
load_dataset <- function(dataframe, row = 1) {
  stopifnot(class(dataframe) == "data.frame")
  stopifnot(nrow(dataframe) >= row && row > 0)

  row_to_load <- dataframe[row,]
  dataesgobr(dataframe = row_to_load)
}

#' @title Download files asociate with dataesgobr object
#' @description This function downloads the data associated with the dataset
#' passed like param from datos.gob.es
#'
#' @param x A dataesgobr containing information and data from datos.gob.es
#' @param format A string, the data's format to download
#' @param all A logical, this parameter indicates if the function must download every file
#' @param position Numeric, the number in the format list
#' @param noconfirm Logical, this parameter indicates if the user must confirm
#' the downloads or no
#' @param overwrite A logical, if this parameter is TRUE then the downloaded file will replace
#' automatically the old file if it exists
#' @param outfile A string, the user can indicates the path to save the file
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' id <- "l01350167-atestados-policia-local-de-las-palmas-de-gran-canaria-2013"
#' dataset <- search_by_id(id)
#' download_data(dataset, "text/csv", noconfirm = TRUE, outfile = tempdir())
#' download_data(dataset, "text/csv", noconfirm = TRUE, overwrite = TRUE, outfile = tempdir())
#' download_data(dataset, "text/csv", FALSE, 3, noconfirm = TRUE)
#' download_data(dataset, "text/csv", FALSE, 3, noconfirm = TRUE, overwrite = TRUE)
#' download_data(dataset, "application/vnd.oasis.opendocument.spreadsheet", TRUE, noconfirm = TRUE, outfile = tempdir())
#' download_data(dataset, "application/vnd.oasis.opendocument.spreadsheet", FALSE, position = 6, noconfirm = TRUE, overwrite = TRUE)
#' }
#' @export
#' @import httr
#' @import readr
#' @import stringr
download_data <- function(x, format, all = TRUE, position = 0, noconfirm = FALSE,
                          overwrite = NULL, outfile = NULL) {
  stopifnot(class(x) == "dataesgobr", is.character(format), is.logical(all),
            is.numeric(position))

  if (is.na(x$formats[format])) {
    message(paste("Error:", format,"format not found."))
    message("If you need to know the available formats about a dataset")
    message("you can use get_available_formats function.")
  } else {
    if (is.null(outfile)) {
      outfile <- tempdir()
    }

    path <- outfile

    if (is.null(overwrite)) {
      confirmOVERWRITE <- FALSE
    } else {
      confirmOVERWRITE <- overwrite
    }

    extension <- get_extension(format)
    cap_speed <- progress(type = c("down", "up"), con = stdout())

    if (all && position == 0) {
      for(element in names(x$formats)) {
        position <- position + 1
        if (format == element) {
          url <- x$formats[position]
          name <- get_name(url, format)

          if (!noconfirm) {
            confirmPATH <- confirm_action(paste0("The file will be save in: ", outfile))
            if (!confirmPATH) {
              path <- readline("Outfile: ")
            } else {
              path <- outfile
            }
            confirmNAME <- confirm_action(paste0("Name: ", name, " | Change name?"))
            if (confirmNAME) {
              name <- readline("New name: ")
            }
            if (file.exists(file.path(path, name))) {
              if (is.null(overwrite)) {
                confirmOVERWRITE <- confirm_action("The file already exists, overwrite?")
              }
            } else {
                confirmOVERWRITE <- TRUE
            }
          } else {
            if (!file.exists(file.path(path, name))) {
              confirmOVERWRITE = TRUE
            }
          }

          if (confirmOVERWRITE) {
            route <- paste0(path, "/", name)
            GET(url, write_disk(route, overwrite = confirmOVERWRITE), progress(),
                cap_speed)
          } else {
            message("Download canceled, the file already exists, set overwrite TRUE")
          }
        }
      }
    } else {
      if (format == names(x$formats)[position]) {
        url <- x$formats[position]
        name <- get_name(url, format)

        if (!noconfirm) {
          confirmPATH <- confirm_action(paste0("The file will be save in: ", outfile))
          if (!confirmPATH) {
            path <- readline("Outfile: ")
          } else {
            path <- outfile
          }
          confirmNAME <- confirm_action(paste0("Name: ", name, " | Change name?"))
          if (confirmNAME) {
            name <- readline("New name: ")
          }
          if (file.exists(file.path(path, name))) {
            if (is.null(overwrite)) {
              confirmOVERWRITE <- confirm_action("The file already exists, overwrite?")
            }
          } else {
            confirmOVERWRITE <- TRUE
          }
        } else {
          if (!file.exists(file.path(path, name))) {
            confirmOVERWRITE = TRUE
          }
        }

        if (confirmOVERWRITE) {
          route <- paste0(path, "/", name)
          GET(url, write_disk(route, overwrite = confirmOVERWRITE), progress(),
              cap_speed)
        } else {
          message("Download canceled, the file already exists, set overwrite TRUE")
        }
      } else {
        warning("The file's format does not match with the parameter format.")
      }
    }
  }
}

#' @title Load data from a file
#' @description This function loads the data from the file passed like param
#'
#' @param file A file with data previously downloaded
#' @param path character The path of the file, you need to use this parameter if
#' the file is located a directory different to working directory
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' dataset <- search_by_id('l01350167-atestados-policia-local-de-las-palmas-de-gran-canaria-2013')
#' download_data(dataset, "text/csv", noconfirm = TRUE)
#' load_data('DB_VEH_2013.csv')
#' }
#' @export
#' @import httr
#' @import readr
#' @import stringr
#' @return A data.frame
load_data <- function(file, path = NULL) {
  stopifnot(is.character(file))
  cap_speed <- progress(type = c("down", "up"), con = stdout())

  format <- get_format(file)
  name <- get_name(file, format)

  if (is.null(path)) {
    path <- getwd()
  }
  setwd(path)

  switch (format,
    "text/csv" = {
      message("Loading csv file.")
      check_csv_file(name, noconfirm = TRUE, path = path)

      symbol <- get_symbol(name)
      content <- read_delim(name, delim = symbol)
    },
    "application/vnd.ms-excel" = {
      message("Loading xls file.")
      content <- as.data.frame(readxl::read_excel(name))
    },
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
      message("Loading xls file.")
      content <- as.data.frame(readxl::read_excel(name))
    },
    "application/vnd.oasis.opendocument.spreadsheet" = {
      message("Loading ods file.")
      content <- readODS::read_ods(name)
    }
  )
  content
}

#' @title Extract the name of the file in the URL
#'
#' @param url A string with URL and the name of the file
#' @param format The data's format to check the extension
#'
#' @examples
#' library(dataesgobr)
#' name <- get_name("archivo-de-datos.csv&rnd=1234", "text/csv")
#' name <- get_name("https://datos.ayto-arganda.es/contratos.pdf", "application/pdf")
#' @export
#' @import stringr
#' @import stringi
#' @return A string with the file's name
get_name <- function(url, format) {
  stopifnot(is.character(url), is.character(format))
  position <- stri_locate_last(url, regex = "/")

  if (is.na(position)[1]) {
    name <- url
  } else {
    name <- substr(url, position+1, 10000)
  }

  extension <- get_extension(format)

  if (str_detect(name, paste0("\\", extension,"$"))) {
    message("Extension detected")
  } else if (str_detect(name, extension)) {
    name <- substr(name, 1, str_locate(name, ".csv")[1,]["end"])
  } else {
    name <- paste0(name, extension)
    warning(paste("Extension not detected, is posible that the url is a hyperlink, \n check the url: ", url))
  }
  name
}

#' @title Get the format that matches with the extension passed like parameter
#'
#' @param ext A string that contains the extension
#'
#' @examples
#' library(dataesgobr)
#' format <- get_format(".csv")
#'
#' format
#'
#' format <- get_format(".pdf")
#'
#' format
#' @return A string
#' @export
get_format <- function(ext) {
  stopifnot(is.character(ext))
  position <- stri_locate_last(ext, regex = "\\.")
  extension <- substr(ext, position, 10000)

  format <- rownames(dataesgobr::datos)[dataesgobr::datos$Extension == extension]
  format
}

#' @title Get the extension that matches with the format passed like parameter
#'
#' @param format A string that contains the format
#'
#' @examples
#' library(dataesgobr)
#' format <- get_extension("text/csv")
#'
#' format
#'
#' format <- get_extension("application/pdf")
#'
#' format
#' @return A string
#' @export
get_extension <- function(format) {
  stopifnot(is.character(format))
  extension <- dataesgobr::datos[format,]
  extension
}

#' @title Get the publisher of the dataset
#'
#' @param id A string with the dataset's ID
#' @export
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' publisher <- get_publisher("L01280066")
#' }
#' @import dplyr
#' @return A data.frame
get_publisher <- function(id) {
  stopifnot(is.character(id))
  data <- data.frame()

  data <- get_publishers_from_api()

  result <- data %>% filter(data["notation"] == id)
  result
}

#' @title This function detects the delim from a csv file
#'
#' @param file The file with the content to check
#'
#' @examples
#' library(dataesgobr)
#' file <- system.file("extdata", "fichero.csv", package="dataesgobr")
#' symbol <- get_symbol(file)
#' @export
#' @import readr
#' @return The symbol as character that split the columns
get_symbol <- function(file) {
  stopifnot(is.character(file))
  symbol <- read_lines(file, n_max = 1)
  if (grepl(";", symbol)) {
    symbol <- ";"
  } else {
    symbol <- ","
  }
}

#' @title Check if the dataset has a correct format
#'
#' @param file The file to check
#' @param path character The path of the file, you need to use this parameter if
#' the file is located a directory different to working directory
#' @examples
#' library(dataesgobr)
#' file <- system.file("extdata", "fichero.csv", package="dataesgobr")
#' correct <- check_file(file)
#' @export
#' @import httr
#' @return Return a logical, if the file is correct it will be TRUE, else FALSE
check_file <- function(file, path = NULL) {
  stopifnot(is.character(file))
  if (is.null(path)) {
    path <- getwd()
  }
  setwd(path)
  result <- FALSE
  if(file.exists(file)) {
    format <- get_format(file)
    switch(format,
           "text/csv" = {
             result <- check_csv_file(file, path = path)
           }
    )
    if (!result) {
      message("Format not supported yet.")
    }
  } else {
    warning("File not found")
    result <- FALSE
  }
  result
}

#' @title Check if the csv file has a correct format
#'
#' @param file The file's path
#' @param noconfirm A logical, this parameter indicates if the user must confirm
#' to change the name of the file and save in different file
#' @param filename A character, if this parameter is present then the name of
#' the file will be automatically set
#' @param path character The path of the file, you need to use this parameter if
#' the file is located a directory different to working directory
#'
#' @return A logical
#' @export
#'
#' @examples
#' library(dataesgobr)
#' file <- system.file("extdata", "fichero.csv", package="dataesgobr")
#' correct <- check_csv_file(file)
check_csv_file <- function(file, noconfirm = FALSE, filename = NULL, path = NULL) {
  stopifnot(file.exists(file))
  content <- read_lines(file)
  vector_complete = vector('character')

  total_lines <- 0
  count_lines <- 0
  confirm <- FALSE

  if(is.null(path)) {
    path <- getwd()
  }
  setwd(path)
  if(file.size(file) == 0) {
    warning("The file is empty")
    correct <- FALSE
  } else {
    message("Checking csv file")
    pb <- txtProgressBar(min = 0, max = length(content))
    for ( i in 1:length(content) ) {
      total_lines = total_lines + 1
      if ( str_detect(content[[i]], "([0-9]|.)(,|;)") ) {
        count_lines = count_lines + 1
        line <- content[[i]]
        line <- str_replace_all(line, "\"", "")
        vector_complete = c(vector_complete, line)
        setTxtProgressBar(pb, i)
      }
    }
    close(pb)

    if ( count_lines == 0 ) {
      warning("Load failed: The file does not have correctly format, please check: ", file)
      correct <- FALSE
    } else if ( total_lines > count_lines ) {
      message(total_lines, " vs ", count_lines)
      warning("The file is not totally correct")
      warning("It will be save but is possible that you can not read this correctly")
      warning("If you have any problem please check: ", file)
      correct <- TRUE
    } else {
      message("\nFile is correct!")
      correct <- TRUE
    }

    if(!is.null(filename)) {
      file <- filename
    } else {
      update <- FALSE
      if(!noconfirm)
        update <- confirm_action(paste("Do you want to change the file's name?"))
      if (update) {
        file <- readline(paste("New name: "))
      }
    }

    if (!noconfirm) {
      confirm <- confirm_action(paste("Do you want to save the modified file:", file))
    }
    if (confirm || noconfirm) {
      write.table(vector_complete, file, row.names = FALSE, col.names = FALSE,
                  quote = FALSE, fileEncoding = "UTF-8")
    }
  }
  correct
}

#' @title Generate a data.frame that contains the type of elements,
#' information and repetitions for each one
#' @export
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' dataesgobrObject <- search_by_id('l01350167-atestados-policia-local-de-las-palmas-de-gran-canaria-2013')
#' formats <- get_available_formats(dataesgobrObject)
#' formats
#' }
#' @param data A dataesgobr object
#' @return A data.frame
get_available_formats <- function(data) {
  stopifnot(class(data) == "dataesgobr")

  formats <- data.frame(stringsAsFactors = FALSE)

  for (i in 1:length(data$formats)) {
    if(length(unlist(data$formats_info[i])) == 0) {
      info <- NA
    } else {
      info <- unlist(data$formats_info[i])
    }
    newFormat <- data.frame(names(data$formats[i]), info, data$formats[[i]])
    formats <- rbind(formats, newFormat)

  }
  names(formats) <- c("Type", "Info", "url")
  formats
}

#' @title Generate a list that contains the type of elements and repetitions for
#' each one
#' @export
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' dataesgobrObject <- search_by_id('l01350167-atestados-policia-local-de-las-palmas-de-gran-canaria-2013')
#' formats <- get_formats(dataesgobrObject)
#' formats
#' }
#' @param data A dataesgobr object
#' @return A list
get_formats <- function(data) {
  stopifnot(class(data) == "dataesgobr")
  list_of_formats <- list()

  for (row in names(data$formats)) {
    for (format in row) {
      if(is.element(format, names(list_of_formats))) {
        list_of_formats[[format]] <- list_of_formats[[format]] + 1
      } else {
        list_of_formats[[format]] <- 1
      }
    }
  }
  list_of_formats
}

#' @title This method extracts the ID from an URL
#' @export
#' @param url A string
#' @examples
#' library(dataesgobr)
#' id <- get_id("https://datos.gob.es/es/catalogo/l01280066-tramites-salud1")
#' id
#' @import stringi
#' @return A string that contains the ID
get_id <- function(url) {
  path <- system.file("url_params.yml", package = "dataesgobr")
  parse <- yaml::read_yaml(path)
  name <- stri_extract(url, regex = parse$parse$id)
  name
}

#' @title This method send a request to datos.gob.es and return a data.frame
#' that contains the themes available in the catalog
#'
#' @export
#' @return A data.frame
get_themes_from_api <- function() {
  data <- data.frame()

  search <- "https://datos.gob.es/apidata/catalog/theme?_pageSize=200&_page=0"
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title This method send a request to datos.gob.es and return a data.frame
#' that contains the spatials available in the catalog
#'
#' @export
#' @return A data.frame
get_spatials_from_api <- function() {
  data <- data.frame()

  search <- "https://datos.gob.es/apidata/catalog/spatial?_pageSize=200&_page=0"
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title This method send a request to datos.gob.es and return a data.frame
#' that contains the publishers in the catalog
#'
#' @export
#' @return A data.frame
get_publishers_from_api <- function() {
  data <- data.frame()

  search <- "https://datos.gob.es/apidata/catalog/publisher?_pageSize=200&_page=0"
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title This method send a request to datos.gob.es and return a data.frame
#' that contains the provinces in the catalog
#'
#' @export
#' @return A data.frame
get_provinces_from_api <- function() {
  data <- data.frame()

  search <- "https://datos.gob.es/apidata/nti/territory/Province?_pageSize=200&_page=0"
  response <- jsonlite::fromJSON(search)

  data <- response[["result"]][["items"]]
}

#' @title Loads the file url_params and make urls to extract information from the API
#'
#' @param field String to specifies the field that we want to search
#' @param request String with the information to search
#' @param params Optional list with the parameters to add to the query
#'
#' @return A string containing the url generated
#' @export
#'
#' @examples
#' library(dataesgobr)
#'
#' # Generates the url associated to the id passed
#' url <- make_url("id", "a13002908-residentes-en-la-comunidad-de-madrid-por-lugar-de-nacimiento")
#'
#' # Generates the url associated to the title and the parameters passed
#' url <- make_url("title", "atestados", c("sort" = "title", "pagesize" = 50, "page" = 1))
make_url <- function(field, request, params = NULL) {
  stopifnot(is.character(field), is.character(request))
  path <- system.file("url_params.yml", package = "dataesgobr")
  urls <- yaml::read_yaml(path)

  if(is.element(field, names(urls$dataset))) {
    url <- paste0(urls$baseurl, urls$catalog)
    switch (field,
            id        = url <- paste0(url, urls$dataset$id),
            title     = url <- paste0(url, urls$dataset$title),
            publisher = url <- paste0(url, urls$dataset$publisher),
            theme     = url <- paste0(url, urls$dataset$theme),
            format    = url <- paste0(url, urls$dataset$format),
            keyword   = url <- paste0(url, urls$dataset$keyword),
            spatial   = url <- paste0(url, urls$dataset$spatial),
            modified  = url <- paste0(url, urls$dataset$modified)
    )
    url <- paste0(url, request)

    if (!is.null(params)) {
      first_parameter <- TRUE
      url <- paste0(url, "?")
      if (is.element("sort", names(params))) {
        first_parameter <- FALSE
        url <- paste0(url, urls$params$sort, params["sort"])
      }

      if (is.element("pagesize", names(params))) {
        if (!first_parameter) {
          url <- paste0(url, "&")
        }
        first_parameter <- FALSE
        url <- paste0(url, urls$params$pagesize, params["pagesize"])
      }

      if (is.element("page", names(params))) {
        if (!first_parameter) {
          url <- paste0(url, "&")
        }
        first_parameter <- FALSE
        url <- paste0(url, urls$params$page, params["page"])
      }
    }
  } else {
    warning("Field not found")
  }

  url
}

#' @title Draw a plot using the pararameters passed
#'
#' @param type String with plot type
#' @param data Data.frame that contains the data to plot
#' @param columns Columns to represent in the plot
#' @param dataSelected Rows selected from the data
#' @param xlim Specifies the limit for x axis
#' @param ylim Specifies the limit for y axis
#' @param nClasses Split data in classes to draw in bloxplot
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' datasets <- search_by_title('atestados')
#' dataset <- load_dataset(datasets)
#' download_data(dataset, "text/csv", TRUE, noconfirm = TRUE)
#' content <- load_data("DB_HER_1999.csv")
#' graph <- graphic_data("plot", content, "SEXO", xlim = 3, ylim = 2000)
#' graph
#' }
#' @return plot
#' @export
#'
graphic_data <- function(type, data, columns, dataSelected = NULL, xlim = NULL,
                         ylim = NULL, nClasses = NULL) {
stopifnot(is.character(type), is.character(columns) || is.vector(columns))

  if (!is.null(dataSelected) && length(columns) == 1) {
    finalData <- data[[columns]][dataSelected]
  } else if (type != "boxplot") {
    finalData <- data[[columns]]
  }

  switch(type,
         "plot" = {
           graph <- plot(as.factor(finalData), col = rainbow(10),
                         xlim = c(0, xlim), ylim = c(0, ylim))
         },
         "hist" = {
           graph <- hist(finalData, nclass = nClasses, col = rainbow(10),
                         xlim = c(0, xlim), ylim = c(0, ylim))
         },
         "pie"  = {
           graph <- pie(table(as.factor(finalData)), col = rainbow(10))
         },
         "boxplot" = {
           column1 <- columns[1]
           column2 <- columns[2]
           graph <- boxplot(data[[column1]] ~ data[[column2]])
         }
        )
  graph
}

#' Generates an interface in R console with a question and options to answer it
#'
#' @param question character containing the question
#' @param options vector containing the options to answer the question
#'
#'
#' @examples
#' \dontrun{
#' confirm_action("Write file memory.csv?")
#' confirm_action("Do you like dataesgobr?", c("Yes" = TRUE, "No" = FALSE, "A bit" = FALSE))
#' }
#' @export
#' @return logical
confirm_action <- function(question, options = NULL) {
  stopifnot(is.character(question))
  if (is.null(options)) {
    opt <- readline(paste(question, " (y/N) > "))
    if (tolower(opt) == "y") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    count <- 0
    for(option in names(options)) {
      count <- count + 1
      message(paste(count, ">" , option))
    }
    opt <- as.numeric(readline(paste("Select an option > ")))
    select <- names(options[opt])
    if(options[select] == TRUE) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
