dataesgobr <- function(url = "",
                       title = "",
                       description = "",
                       formats,
                       format_links,
                       issued,
                       identifier,
                       keywords,
                       publisher,
                       spatial,
                       theme,
                       json,
                       create_from_json = !missing(json), ...) {

  stopifnot(is.character(url))

  if (create_from_json) {
    do.call(dataesgobr_from_json, c(json))
  } else {
    value <- list(
      url = url,
      title = title,
      description = description,
      issued = issued,
      identifier = identifier,
      formats = format_links,
      keywords = keywords,
      publisher = publisher,
      spatial = spatial,
      theme = theme)

    names(value$formats) = formats
    attr(value, "class") <- "dataesgobr"

    value
  }
}

dataesgobr_from_json <- function(json) {
  if ( is.null(nrow(json$distribution)) ) {
    values <- json$distribution[[1]]$format$value
    access <- json$distribution[[1]]$accessURL
  } else {
    values <- json$distribution$format$value
    access <- json$distribution$accessURL
  }

  value <- list(
    url = as.character(json["_about"]),
    title = as.character(json["title"]),
    description = as.character(json$description[[1]]),
    formats = access,
    issued = json$issued,
    identifier = json$identifier,
    keywords = json$keyword,
    publisher = json$publisher,
    spatial = json$spatial,
    theme = json$theme
  )

  names(value$formats) = values
  attr(value, "class") <- "dataesgobr"

  value
}
