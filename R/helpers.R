


#' @export
#' @importFrom ggplot2 theme_set
make_beautiful_plots <- function() {
  ggplot2::theme_set(zibas::theme_ziba())
}

#' @export
#' @import ggplot2
extend_right <- function(cm) {
  c <- 0.08
  cr <- 1.05 + ((cm / 1.5) - 1) * c
  ggplot2::theme(
    plot.margin = ggplot2::unit(c(0.5, cm, 0.5, 0.5), "cm"),
    plot.caption = ggplot2::element_text(hjust = cr)
  )
}


#' @export
#' @importFrom ggplot2 labs
plot_source <- function(source, last_updated = format(Sys.Date(), "%B %Y"), linebreak = F) {
  stopifnot(inherits(source, "character"), inherits(last_updated, "character"))
  split <- ifelse(linebreak, ".\n", ". ")
  ggplot2::labs(caption = paste0("Source: ", source, split, "Last updated ", last_updated, "."))
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


#' @export
set_colours <- function(colour_vector, ...) {
  scale_colour_manual(..., values = colour_vector)
}

#' @export
set_fills <- function(colour_vector, ...) {
  scale_fill_manual(..., values = colour_vector)
}

#' @export
rev_major_lines <- function() {
  theme(panel.grid.major.x = ggplot2::element_line(
    linetype = "dotted",
    colour = "grey60",
    size = 0.2
  ), panel.grid.major.y = ggplot2::element_blank())
}

#' @export
set_legend_rows <- function(rows) {
  guides(colour = guide_legend(nrow = rows))
}
