


#' @export
#' @importFrom ggplot2 theme_set
make_beautiful_plots <- function() {
  ggplot2::theme_set(zibas::theme_ziba())
}

#' @export
#' @import ggplot2
extend_right <- function(cm, caption_hjust = 1.15) {
  ggplot2::theme(
    plot.margin = ggplot2::unit(c(0.5, cm, 0.5, 0.5), "cm"),
    plot.caption = ggplot2::element_text(hjust = caption_hjust)
  )
}

#' @export
#' @importFrom ggplot2 labs
plot_source <- function(source, last_updated = format(Sys.Date(), "%B %Y")) {
  stopifnot(inherits(source, "character"), inherits(last_updated, "character"))
  ggplot2::labs(caption = paste0("Source: ", source, ". Last updated ", last_updated, "."))
}

#' @export
no_clip <- function() coord_cartesian(clip = "off")

#' @export
flip <- function(...) coord_flip(...)

#' @export
no_axis_labs <- function() labs(x = "", y = "")

#' @export
no_leg <- function() theme(legend.position = "none")

#' @export
quick_labs <- function(title, subtitle = "") {
  labs(x = "", y = "", title = title, subtitle = subtitle)
}
