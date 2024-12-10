roxify <- function(data, descriptions = NULL) {
  items <- names(data)
  classes <- data %>% purrr::map_chr(.f = function(x) class(x)[1])
  if (!is.null(descriptions)) {
    dframe <- data.frame(items, classes)
    dframe <- dframe %>% left_join(descriptions, by=c("items"="name"))
    dframe$concept <- ifelse(is.na(dframe$concept), "", paste0(", ", dframe$concept))
    dframe$label <- ifelse(is.na(dframe$label), "", paste0(", ", dframe$label))
    classes <- paste0(dframe$classes, dframe$concept, dframe$label)
  }
  itemlist <- paste0("#\'   \\item{",items,"}{", classes, "}", sep="", collapse="\n")
  cat(sprintf("#\' @format A tibble with %s rows and %s columns
#\' \\describe{
%s
#\' }
", nrow(data), ncol(data), itemlist))
}



