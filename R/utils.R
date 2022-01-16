#' @importFrom magrittr %>%

`%+%` <- function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste0(a, b)
  } else {
    stop("%+% requires two strings")
  }
}

`%||%` <- function(a, b) ifelse(!is.null(a), a, b)