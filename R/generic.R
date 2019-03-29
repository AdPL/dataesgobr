print.dataesgobr <- function(obj) {
  cat("Title:", obj$title, "\n")
  cat("Description:", obj$description, "\n")
  cat("URL:", obj$url, "\n")
  cat("Available formats:", names(obj$formats), "\n")
}
