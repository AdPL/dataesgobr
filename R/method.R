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
#' mydataesgobr <- dataesgobr_search_by_title('puente')
#'
#' # Return the first 78 matches that contain puente in their title
#' mydataesgobr <- dataesgobr_search_by_title('puente', 78)
#'
#' }
#' @export
#' @return A data.frame containing information about datasets that match with
#' the title param
search_by_title <- function(title, numentry = 50, page = 0) {
  stopifnot(is.character(title), is.numeric(numentry))
  data <- data.frame()

  search <- paste("https://datos.gob.es/apidata/catalog/dataset/title/",
                  title, "?_pageSize=", numentry, "&_page=", page, sep = "")
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
#' dataesgobr_search_by_id('l01280066-presupuestos-20141')
#' }
#' @export
#' @return A dataesgobr object
search_by_id <- function(id) {
  stopifnot(is.character(id))

  search <- paste("https://datos.gob.es/apidata/catalog/dataset/", id, sep = "")
  response <- jsonlite::fromJSON(search)
  datos <- response[["result"]][["items"]]

  if (length(datos) == 0) {
    datos <- NULL
  } else {
    dataesgobr_from_json(json = datos)
  }
}

#' @title Filter data.frame by title using q param
#'
#' @param data A data.frame that will be filtered
#' @param q A string to match in data.frame data
#' @param quiet Set if the function can print info messages or not
#' @import dplyr
#' @import stringr
#' @export
#' @return A data.frame with rows that matches the dataset title
filter_by_title <- function(data, q, quiet = FALSE) {
  stopifnot(class(data) == 'data.frame', class(q) == 'character')

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
#' @export
#' @return A data.frame with rows that matches the description
filter_by_description <- function(data, q, quiet = FALSE) {
  stopifnot(class(data) == 'data.frame', class(q) == 'character')

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
#' @export
#' @return A data.frame that matches any given keyword
filter_by_keywords <- function(data, keywords, quiet = FALSE) {
  stopifnot(class(data) == 'data.frame', class(keywords) == 'character')

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
#' @import graphics
#' @import grDevices
#' @import utils
graphic_keywords <- function(list, type = "circular", nrepeats = 0) {
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
#' than one element this param determinates the row to load
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
#' @param x dataesgobr containing information and data from datos.gob.es
#' @param format The data's format to download
#' @param all This parameter indicates if the function must download every file
#' @param position The number in the format list
#'
#' @export
#' @import httr
#' @import readr
download_data <- function(x, format, all = TRUE, position = 0) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }

  stopifnot(class(x) == 'dataesgobr')

  if (is.na(x$formats[format])) {
    message(paste("Error:", format,"format not found."))
    message("If you need to know the available formats about a dataset")
    message("you can use get_available_formats function.")
  } else {
    extension <- get_extension(format)
    cap_speed <- progress(type = c("down", "up"), con = stdout())
    if (all) {
      position <- 0
      for(element in names(x$formats)) {
        position <- position + 1
        if (format == element) {
          url <- x$formats[position]
          name <- get_name(url, format)

          if (!file.exists(name)) {
            message(paste("Downloading: ", name))
            GET(url, write_disk(name, overwrite = TRUE),
                progress(), cap_speed)
          }
        }
      }
    } else {
      url <- x$formats[position]
      name <- get_name(url, format)

      if (!file.exists(name)) {
        message(paste("Downloading: ", name))
        GET(url, write_disk(name, overwrite = TRUE),
            progress(), cap_speed)
      }
    }
  }
}

#' @title Load data from a file
#' @description This function loads the data from the file passed like param
#'
#' @param file A file with data previously downloaded
#' @export
#' @import httr
#' @import readr
#' @return A data.frame
load_data <- function(file) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }

  cap_speed <- progress(type = c("down", "up"), con = stdout())

  format <- get_format(file)
  name <- get_name(file, format)

  switch (format,
    "text/csv" = {
      message("Loading csv file.")
      check_csv_file(name)

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
#' @export
#' @import stringr
#' @import stringi
#' @return A string with the file's name
get_name <- function(url, format) {
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
#' @return A string
#' @export
get_format <- function(ext) {
  position <- stri_locate_last(ext, regex = "\\.")
  extension <- substr(ext, position, 10000)

  format <- rownames(dataesgobr::datos)[dataesgobr::datos$Extension == extension]
  format
}

#' @title Get the extension that matches with the format passed like parameter
#'
#' @param format A string that contains the format
#'
#' @return A string
#' @export
get_extension <- function(format) {
  extension <- dataesgobr::datos[format,]
  extension
}

#' @title Get the publisher of the dataset
#'
#' @param id A string with the dataset's ID
#' @export
#' @import dplyr
#' @return A string that contains the publisher's name
get_publisher <- function(id = "") {
  stopifnot(is.character(id))
  data <- data.frame()

  search <- paste0("https://datos.gob.es/apidata/catalog/publisher?_sort=notation&_pageSize=200&_page=0")
  response <- jsonlite::fromJSON(search)
  data <- response[["result"]][["items"]]

  if (id == "") {
    data
  } else {
    result <- data %>% filter(data["notation"] == id)
    result
  }
}

#' @title This function detects the delim from a csv file
#'
#' @param file The file with the content to check
#' @import readr
#' @return The symbol as character that split the columns
get_symbol <- function(file) {
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
#' @export
#' @import httr
#' @return Return a logical, if the file is correct it will be TRUE, else FALSE
check_file <- function(file) {
  result <- FALSE
  if(file.exists(file)) {
    format <- get_format(file)
    switch(format,
           "text/csv" = {
             result <- check_csv_file(file)
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

check_csv_file <- function(file) {
  stopifnot(file.exists(file))
  content <- read_lines(file)
  vector_complete = vector('character')

  total_lines <- 0
  count_lines <- 0

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
    write.table(vector_complete, file, row.names = FALSE, col.names = FALSE,
                quote = FALSE, fileEncoding = "UTF-8")
  }
  correct
}

#' @title Generate a data.frame that contains the type of elements, information and repetitions for each one
#' @export
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

#' @title Generate a list that contains the type of elements and repetitions for each one
#' @export
#' @param data A dataesgobr object
#' @return A list
get_formats <- function(data) {
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
#' @import stringi
#' @return A string that contains the ID
get_id <- function(url) {
  position <- stri_locate_last(url, regex = "/")
  name <- substr(url, position+1, 10000)
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
