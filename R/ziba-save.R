# Build a filename from the plot title, or the next free figureNNN in path
default_filename <- function(plot, path) {
  title <- plot$labels$title

  if (is.character(title) && length(title) == 1 && nzchar(title)) {
    # lowercase, collapse non-alphanumerics to hyphens, trim, cap length
    slug <- gsub("[^a-z0-9]+", "-", tolower(title))
    slug <- gsub("^-+|-+$", "", slug)
    return(substr(slug, 1, 60))
  }

  # no usable title: find the highest existing figureNNN and add one
  existing <- list.files(path, pattern = "^figure[0-9]{3}\\.")
  n <- if (length(existing) > 0) max(as.integer(substr(existing, 7, 9))) + 1 else 1
  sprintf("figure%03d", n)
}


# Return the graphics device for a format. ragg and svglite read system fonts
# directly, so Matter renders without any font registration.
ziba_device <- function(format, outline_text = FALSE) {
  if (format == "png") {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      stop("Saving PNG requires the 'ragg' package.", call. = FALSE)
    }
    return(ragg::agg_png)
  }

  # Cairo svg converts text to paths, so the font survives on machines
  # without Matter, but the text is no longer editable in PowerPoint
  if (outline_text) {
    return(grDevices::svg)
  }

  if (!requireNamespace("svglite", quietly = TRUE)) {
    stop("Saving SVG requires the 'svglite' package.", call. = FALSE)
  }
  svglite::svglite
}


#' Save a ziba plot
#'
#' Wrapper for `ggplot2::ggsave()` with export presets. During the save it
#' swaps the global theme for `theme_ziba(base_size = base_size)`, so text
#' scales to suit the output size while per-plot `+ theme()` tweaks still
#' apply.
#'
#' @param plot The plot to save. Defaults to the last plot.
#' @param filename File name without extension. If NULL, the function uses a
#'   slug of the plot title, or the next free "figureNNN" if there is no title.
#' @param path Directory to save into; created if it doesn't exist.
#' @param format "png" (default), "svg", or c("png", "svg") to save both.
#' @param width Width in inches.
#' @param height Height in inches. If NULL, the function calculates it as
#'   `width / aspect`.
#' @param aspect Width-to-height ratio, used only when `height` is NULL.
#' @param base_size Base font size for the export theme. Set to NULL to
#'   save with the plot's current theme unchanged.
#' @param bg Plot background fill. Defaults to "transparent". Set to NULL
#'   to keep the theme's background colour.
#' @param dpi Resolution for PNG output.
#' @param outline_text For SVG only. If TRUE, the function converts text to
#'   paths so the font renders on machines without Matter installed.
#' @param ... Other arguments passed to `ggplot2::ggsave()`.
#'
#' @return The saved file paths, invisibly.
#' @export
#' @examples
#' \dontrun{
#' p |> ziba_save()
#' p |> ziba_save("northland-upgrades", path = "export/charts", format = c("png", "svg"))
#' p |> ziba_save(width = 6.3, aspect = 4 / 3)
#' }
ziba_save <- function(plot = ggplot2::last_plot(),
                      filename = NULL,
                      path = ".",
                      format = "png",
                      width = 8,
                      height = NULL,
                      aspect = 16 / 9,
                      base_size = 8,
                      bg = "white",
                      dpi = 300,
                      outline_text = FALSE,
                      ...) {
  format <- match.arg(format, c("png", "svg"), several.ok = TRUE)

  if (is.null(height)) {
    height <- width / aspect
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # strip any extension the caller passed; format controls the extension
  if (is.null(filename)) {
    filename <- default_filename(plot, path)
  }
  filename <- tools::file_path_sans_ext(filename)

  # swap in an export-sized global theme and restore the old one on exit.
  # Per-plot + theme() changes are stored on the plot, so they still apply.
  if (!is.null(base_size)) {
    old_theme <- ggplot2::theme_set(theme_ziba(base_size = base_size))
    on.exit(ggplot2::theme_set(old_theme), add = TRUE)
  }

  # override the plot background so transparent export works over the theme fill
  if (!is.null(bg)) {
    plot <- plot + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA)
    )
  }

  files <- file.path(path, paste0(filename, ".", format))

  for (i in seq_along(format)) {
    ggplot2::ggsave(
      filename = files[i],
      plot = plot,
      device = ziba_device(format[i], outline_text),
      width = width,
      height = height,
      units = "in",
      dpi = dpi,
      bg = bg,
      ...
    )
  }

  invisible(files)
}