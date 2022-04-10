

#' Save your Zibas plot
#'
#' Wrapper for ggplot2's save function with presets that work with the font family.
#'
#' @param filename desired name of file on disk
#'
#' @import ggplot2 stringr ragg
#' @export
#'
zibasave <- function(filename,
                     plt = ggplot2::last_plot(), path = ".",
                     pdf = FALSE, ...) {
  if (path == "." && !is.null(options("zibas.savepath"))) {
    path <- options("zibas.savepath")
    cli::cli_alert_info("Saving plot to {.path {path}}")
  }

  filename <- stringr::str_replace_all(filename, "_", "-")
  width <- 10

  format <- options("zibas.format")
  if (!is.null(format) && format == "pdf" | pdf) {
    device <- cairo_pdf
    ext <- ".pdf"
  } else {
    device <- ragg::agg_png()
    ext <- ".png"
  }

  ggplot2::ggsave(
    paste0(filename, ext),
    device = device,
    plot = plt,
    path = path,
    width = width,
    height = width / 1.618,
    units = "in"
  )
}