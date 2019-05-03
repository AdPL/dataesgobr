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
#' @import jsonlite
#' @return A data.frame containing information about datasets that match with
#' the title param
search_by_title <- function(title, numentry = 50, page = 0) {
  stopifnot(is.character(title), is.numeric(numentry))
  data <- data.frame()

  search <- paste("https://datos.gob.es/apidata/catalog/dataset/title/",
                  title, "?_pageSize=", numentry, "&_page=", page, sep = "")
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
  stopifnot(is.character(id))

  search <- paste("https://datos.gob.es/apidata/catalog/dataset/", id, sep = "")
  response <- fromJSON(search)
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
#'
#' @export
#' @import httr
#' @import readr
#' @return
download_data <- function(x, format) {
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
    switch(format,
           "text/csv" = { extension <- ".csv" },
           "application/pdf" = { extension <- ".pdf" },
           "application/vnd.ms-excel" = { extension <- ".xls" },
           "application/json" = { extension <- ".json" })

    cap_speed <- progress(type = c("down", "up"), con = stdout())
    position <- 0
    for(element in names(x$formats)) {
      position <- position + 1
      if (format == element) {
        url <- x$formats[position]
        name <- get_name(url)

        if (is.na(stri_locate_last(url, regex = extension)[[1]])) {
          name <- paste0(name, extension)
        }

        if (!file.exists(name)) {
          message(paste("Downloading: ", name))
          GET(url, write_disk(name, overwrite = TRUE),
              progress(), cap_speed)
        }
      }
    }
  }
}

#' @title Load data from a file
#' @description This function loads the data from the file passed like param
#'
#' @param x A file with data previously downloaded
#' @export
#' @import httr
#' @import readr
#' @return A data.frame
load_data <- function(file) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work.
         Please install it.", call. = FALSE)
  }

}

#' @title Extract the name of the file in the URL
#'
#' @param url A string with URL and the name of the file
#' @param format The data's format to check the extension
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
  }
  name
}

get_format <- function(file) {
  position <- stri_locate_last(file, regex = "\\.")
  extension <- substr(file, position, 10000)

  switch(extension,
         ".csv" = { format <- "text/csv" },
         ".pdf" = { format <- "application/pdf" },
         ".xls" = { format <- "application/vnd.ms-excel" },
         ".json" = { format <- "application/json" })
  format
}

get_extension <- function(format) {
  switch(format,
         "text/csv" = { extension <- ".csv" },
         "application/pdf" = { extension <- ".pdf" },
         "application/vnd.ms-excel" = { extension <- ".xls" },
         "application/json" = { extension <- ".json" })
  extension
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
#' @import httr
#' @return Return a logical, if the file is correct it will be TRUE, else FALSE
check_file <- function(file) {
  if (url.exists(file)) {
    name <- get_name(file)
    if (file.exists(name)) {
      message("The file already exists")
      result <- check_csv_file(name)
    } else {
      message("Downloading file")
      cap_speed <- progress(type = c("down", "up"), con = stdout())
      GET(file, write_disk(name, overwrite = TRUE), progress(), cap_speed)
      result <- check_csv_file(name)
    }
  } else {
    if(file.exists(file)) {
      message("The file already exists")
      result <- check_csv_file(file)
    } else {
      warning("File not found")
      result <- FALSE
    }
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

#' @title Print available formats in the dataset
#'
#' @param data A dataesgobr object
#' @export
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
