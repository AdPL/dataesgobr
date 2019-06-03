prepare_descriptions <- function(description_dataset) {
  descriptions <- as.character(unlist(description_dataset))
  total_languages <- length(descriptions)

  description <- list(descriptions[0:(total_languages/2)])
  languages <- list(descriptions[((total_languages/2)+1):total_languages])

  final_languages <- unlist(description)
  names(final_languages) <- unlist(languages)

  final_languages
}

update_dates <- function(df) {
  columns <- sapply(df, function(x) !all(is.na(as.Date(as.character(x), format="%d/%m/%Y"))))
  for (column in names(columns)) {
    if (columns[column] == TRUE) {
      df = mutate_each(df, funs(as.Date), column)
    }
  }
  df
}

prepare_titles <- function(titles_dataset) {
  titles <- unlist(titles_dataset)
  title <- titles[1]
  title
}
