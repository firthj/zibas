#' Save your Zibas plot
#'
#' Wrapper for ggplot2's save function with presets that work with the font family.
#'
#' @param filename desired name of file on disk. If NULL, defaults to plot title, if there is one, otherwise "figure%03d".
#' @param plot The plot to print, defaults to last plot
#' @param path The path to save the file to
#' @param width The width of the plot
#' @param height The height of the plot
#' @param r The right margin of the plot
#' @param cr The right margin of the caption
#' @param bg The background color of the plot
#' @param ... Additional arguments to pass to ggsave
#' @import ggplot2 stringr cli
#' @export
#'
#' @export
zibasave <- function(filename = NULL, path = ".", height = 6.5,
                     plt = ggplot2::last_plot(), r = 0.8, cr = 1,
                     bg = "transparent", ...) {
  plt <- plt + theme(
    plot.margin = grid::unit(c(0.2, r, 0.2, 0.2), "cm"),
    plot.title = ggplot2::element_text(
      size = 12, colour = .state$colour,
      hjust = 0, margin = ggplot2::margin(l = 2, b = 2, t = 0),
      face = "bold", family = "Segoe UI",
    ),
    plot.subtitle = ggplot2::element_text(
      size = 8, colour = "grey30",
      hjust = 0.003, family = "Segoe UI",
      margin = ggplot2::margin(t = 0, b = 5)
    ),
    plot.caption = ggplot2::element_text(
      size = 6, colour = "grey30",
      hjust = cr, family = "Segoe UI",
      margin = ggplot2::margin(t = 1, b = 0, r = 0)
    ),
    axis.title = ggplot2::element_text(
      size = 8, colour = "gray40",
      hjust = 0.95
    ),
    axis.line = ggplot2::element_line(
      color = "gray80", size = 0.3,
    ),
    axis.text = ggplot2::element_text(size = 8, colour = "gray40"),
    plot.background = element_rect(fill = bg, , colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 6, colour = "gray40"),
    strip.text = ggplot2::element_text(size = 4, colour = "gray40"),
    legend.key.size = unit(0.2, "cm"),
  )
  save_plot(plt, filename, path, width, height)
}

#' @export
zibasave_ridges <- function(filename = NULL, path = ".", height = 6.5,
                            plt = ggplot2::last_plot(), r = 0.8, cr = 1,
                            bg = "transparent") {
  plt <- plt + theme(
    plot.margin = grid::unit(c(0.2, r, 0.2, 0.2), "cm"),
    plot.title = ggplot2::element_text(
      size = 12, colour = .state$colour,
      hjust = 0, face = "bold", family = "Segoe UI",
      margin = ggplot2::margin(l = 2, b = 2, t = 0)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 8, colour = "grey30",
      hjust = 0.003, family = "Segoe UI",
      margin = ggplot2::margin(t = 0, b = 5)
    ),
    plot.caption = ggplot2::element_text(
      size = 6, colour = "grey30",
      hjust = cr, family = "Segoe UI",
      margin = ggplot2::margin(t = 2, b = 0, r = 0)
    ),
    axis.title = ggplot2::element_text(
      size = 8, colour = "gray40"
    ),
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      size = 8, colour = "gray40",
      hjust = 0.95, margin = ggplot2::margin(t = -18, b = 0, r = 0)
    ),
    axis.text.y = ggplot2::element_text(size = 8, colour = "gray40"),
    plot.background = element_rect(fill = bg, , colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 6, colour = "gray40"),
    strip.text = ggplot2::element_text(size = 4, colour = "gray40"),
    legend.key.size = unit(0.2, "cm"),
  )
  save_plot(plt, filename, path, width, height)
}

save_plot <- function(plt, filename, path, width, height) {
  width <- 9.1
  if (nchar(plt$labels$x) == 0) {
    plt <- plt + theme(
      axis.title.x = element_blank()
    )
  }

  if (nchar(plt$labels$y) == 0) {
    plt <- plt + theme(
      axis.title.y = element_blank()
    )
  }
  if (is.null(filename)) {
    if (!is.null(plt$labels$title)) {
      filename <- plt$labels$title
    } else {
      filename <- paste0("figure-saved-", Sys.time())
    }
  }

  filename <- stringr::str_replace_all(tolower(filename), "_", "-")
  filename <- stringr::str_replace_all(filename, " ", "-")
  if (nchar(filename) > 40) filename <- str_ext(filename, "^.{40}")
  ggplot2::ggsave(
    paste0(filename, ".png"),
    path = path,
    plot = ggplot2::last_plot(),
    width = width,
    height = height,
    units = "cm"
  )
  filename
}
