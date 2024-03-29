

#' Save your Zibas plot
#'
#' Wrapper for ggplot2's save function with presets that work with the font family.
#'
#' @param filename desired name of file on disk. If NULL, defaults to plot title, if there is one, otherwise "figure%03d".
#' @param plot The plot to print, defaults to last plot
#' @param pdf Use pdf format instead of default png
#'
#' @import ggplot2 stringr cli
#' @export
#'
zibasave <- function(filename = NULL,
                     plot = ggplot2::last_plot(), path = ".",
                     pdf = FALSE,
                     bg = "white") {
  width <- 10
  if (is.null(filename)) {
    if (!is.null(plot$labels$title)) {
      filename <- plot$labels$title
    } else {
      filename <- paste0("figure-saved-", Sys.time())
    }
  }
  filename <- stringr::str_replace_all(tolower(filename), "_", "-")
  filename <- stringr::str_replace_all(filename, " ", "-")

  format <- options("zibas.format")
  if (!is.null(format) && format == "pdf" | pdf) {
    device <- cairo_pdf
    ext <- ".pdf"
  } else {
    device <- NULL
    ext <- ".png"
  }


  plot <- plot + theme(
    plot.background = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA)
  )


  ggplot2::ggsave(
    paste0(filename, ext),
    device = device,
    plot = plot,
    path = path,
    width = width,
    height = width / 1.618,
    units = "in"
  )
}
