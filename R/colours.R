
#' Create a blue gradient
#'
#' Extends RColorBrewer with functionality for more than 9 shades of blue.
#'
#' @param n number of shades.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#'
#'
#' @return character vector of `n` colours in hexidecimal format.
#' @export
ziba_gradient <- function(n) {
  blues <- c(RColorBrewer::brewer.pal(9, "Blues")[3:7], "#023e8a")
  grDevices::colorRampPalette(blues, bias = 1.5, space = "Lab")(n)
}



#' A Beautiful blue fill gradient for ggplots
#'
#' Blue fill gradient for use in ggplots
#'
#' @param ... All the usual parameters that apply to a fill scale.
#' @param continuous Whether the data is continuous or discrete.
#'
#' @import ggplot2
#' @export
#'
#' @return A ggplot
#' @examples
#' # ADD_EXAMPLES_HERE
scale_fill_ziba_blue <- function(..., continuous = FALSE) {
  if (continuous) {
    return(ggplot2::scale_fill_gradientn(..., colours = blue_gradient(256)))
  }
  return(ggplot2::discrete_scale("fill", "zibas_blue", blue_gradient, ...))
}

#' A Beautiful blue colour gradient for ggplots
#'
#' Blue colour gradient for use in ggplots
#'
#' @param ... All the usual parameters that apply to a colour scale.
#' @param continuous Whether the data is continuous or discrete.
#'
#' @import ggplot2
#' @export
#'
#' @return A ggplot
#' @examples
#' # ADD_EXAMPLES_HERE
scale_colour_ziba_blue <- function(..., continuous = FALSE) {
  if (continuous) {
    return(ggplot2::scale_colour_gradientn(..., colours = blue_gradient(256)))
  }
  return(ggplot2::discrete_scale("colour", "zibas_blue", blue_gradient, ...))
}



#' A beautiful blue
#'
#' Returns standard ziba blue for consistency
#'
#' @export
zibablue <- function() "#023e8a"


#' A beautiful red
#'
#' Returns standard ziba red for consistency
#'
#' @export
zibared <- function() "#d62828"

#' @export
zibagray <- function() "gray60"

#' @export
zibadarkgray <- function() "gray30"