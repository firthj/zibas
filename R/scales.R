#' Easy number scales.
#'
#' Quickly format big.marks to commas, and scale numbers to metric units.
#' @name num_scale
#' @param scale Shorthand number in scientific format, must be either 1, 1e3,1e6, or 1e9 to be recognised. Will be inverted in the scale (1/1e3).
#' @param ... arguments passed on to ggplot2::scale_y_continuous.
#'
#' @importFrom scales number_format

#' @rdname num_scale
#' @export
scale_y_number <- function(scale = 1, ...) {
  if (scale / 1e3 == 1) {
    suffix <- "k"
  } else if (scale / 1e6 == 1) {
    suffix <- "M"
  } else if (scale / 1e9 == 1) {
    suffix <- "B"
  } else {
    suffix <- ""
  }
  ggplot2::scale_y_continuous(...,
    labels = scales::number_format(
      scale = 1 / scale,
      suffix = suffix,
      big.mark = ","
    )
  )
}

#' @rdname num_scale
#' @export
scale_x_number <- function(scale = 1, ...) {
  if (scale / 1e3 == 1) {
    suffix <- "k"
  } else if (scale / 1e6 == 1) {
    suffix <- "M"
  } else if (scale / 1e9 == 1) {
    suffix <- "B"
  } else {
    suffix <- ""
  }
  ggplot2::scale_x_continuous(...,
    labels = scales::number_format(
      scale = 1 / scale,
      suffix = suffix,
      big.mark = ","
    )
  )
}

#' @importFrom scales percent_format
#' @export
scale_y_percent <- function(...) {
  ggplot2::scale_y_continuous(...,
    labels = scales::percent_format()
  )
}

scale_fill_ziba_blue <- function(..., continuous = FALSE) {
  if (continuous) {
    return(ggplot2::scale_fill_gradientn(..., colours = ziba_gradient(256)))
  }
  return(ggplot2::discrete_scale("fill", "zibas_blue", ziba_gradient, ...))
}
