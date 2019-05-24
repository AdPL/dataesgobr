prepare_descriptions <- function(description_dataset) {
  descriptions <- as.character(unlist(description_dataset))
  total_languages <- length(descriptions)

  description <- list(descriptions[0:(total_languages/2)])
  languages <- list(descriptions[((total_languages/2)+1):total_languages])

  final_languages <- unlist(description)
  names(final_languages) <- unlist(languages)

  final_languages
}
