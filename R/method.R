#' @title Creates a data.frame containing datasets from datos.gob.es
#' @description Send a request to datos.gob.es using the title param
#' to search datasets that match with the title, then the results are returned
#' as data.frame
#'
#' @param title Title to search
#' @param numentry Number of datasets
#'
#' @examples
#' library(dataesgobr)
#' \dontrun{
#' # Return first 50 matches that contain puente in their title
#' mydataesgobr <- dataesgobr_search_by_title('puente')
#'
#' # Return the first 78 matches that contain puente in their title and
#' mydataesgobr <- dataesgobr_search_by_title('puente', 78)
#'
#' }
#' @export
#' @import jsonlite
#' @return A data.frame containing information about datasets that match with
#' the title param
search_by_title <- function(title, numentry = 50) {
  stopifnot(is.character(title), is.numeric(numentry))
  data <- data.frame()

  search <- paste("https://datos.gob.es/apidata/catalog/dataset/title/",
                  title, "?_page=0&_pageSize=", numentry, sep = "")
  response <- fromJSON(search)

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
  search <- paste("https://datos.gob.es/apidata/catalog/dataset/", id, sep = "")
  response <- fromJSON(search)
  datos <- response[["result"]][["items"]]

  dataesgobr_from_json(json = datos)
}

#' @title Filter data.frame by title using q param
#'
#' @param data A data.frame that will be filtered
#' @param q A string to match in data.frame data
#' @export
#' @return A data.frame with rows that matches the dataset title
filter_by_title <- function(data, q) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
    }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }

  stopifnot(class(data) == 'data.frame', class(q) == 'character')

  result <- dplyr::filter(data, stringr::str_detect(
    stringr::str_to_lower(unlist(as.character(data$title))),
    stringr::str_to_lower(q)))

  nMatches <- nrow(result)
  message("Found ", nMatches, " matches.")
  result
}

#' @title Filter data.frame by description using q param
#'
#' @param data A data.frame that will be filtered
#' @param q A string to match in data.frame data
#' @export
#' @return A data.frame with rows that matches the description
filter_by_description <- function(data, q) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }

  stopifnot(class(data) == 'data.frame', class(q) == 'character')

  result <- dplyr::filter(data, stringr::str_detect(
    stringr::str_to_lower(unlist(as.character(data$description))),
    stringr::str_to_lower(q)))

  nMatches <- nrow(result)
  message("Found ", nMatches, " matches.")

  result
}

#' @title Filter data.frame by keywords using keywords param
#'
#' @param data A data.frame that will be filtered
#' @param keywords A string to match in data.frame data
#' @export
#' @import dplyr
#' @return A data.frame that matches any given keyword
filter_by_keywords <- function(data, keywords) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }

  stopifnot(class(data) == 'data.frame', class(keywords) == 'character')
  result <- dplyr::filter(data, stringr::str_detect(
    stringr::str_to_lower(unlist(as.character(data$keyword))),
    stringr::str_to_lower(keywords)))
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

#' @title Load a dataset asociate with dataesgobr object
#' @description This function downloads the data associated with the dataset
#' passed like param and downloads from datos.gob.es
#'
#' @param x dataesgobr containing information and data from datos.gob.es
#' @export
#' @import httr
#' @import stringr
#' @import readr
#' @import stringi
#' @return A data.frame containing data from datos.gob.es
load_data <- function(x) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }
  stopifnot(class(x) == 'dataesgobr')

  if (is.na(x$formats['text/csv'])) {
    cat("Error: csv data not found.")
  } else {
    url <- x$formats['text/csv']
    position <- stri_locate_last(url, regex = "/")
    name <- substr(url, position+1, 10000)


    if (str_detect(name, ".csv$")) {
      message("Extension detected")
      message("Downloading file: ", name, "\n")

      cap_speed <- progress(type = c("down", "up"), con = stdout())
      GET( url, write_disk(name, overwrite = TRUE), progress(), cap_speed)

      message("\nChecking file")
      content <- read_lines(name)
      vector_complete = vector('character')

      total_lines <- 0
      count_lines <- 0

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
        stop("Load failed: The file does not have correctly format,
             please check: ", name)
      } else if ( total_lines > count_lines ) {
        message(total_lines, " vs ", count_lines)
        warning("The file is not totally correct")
        warning("It will be save but is possible that you can not read
                this correctly")
        warning("If you have any problem please check: ", name)
      } else {
        message("\nFile is correct!")
      }
      write.table(vector_complete,
                  name,
                  row.names = FALSE,
                  col.names = FALSE,
                  quote = FALSE,
                  fileEncoding = "UTF-8")
      datos <- read_csv(name)
    } else {
      message("This function is not implemented yet")
    }
  }
}
