#' Resolve a font family, falling back if it isn't installed
#'
#' Checks the system font database for the requested family and warns once
#' per session if it is missing, so charts still render on machines without
#' the brand font.
#'
#' @param family Font family name to look for.
#' @param fallback Family to use if `family` is not installed.
#' @return A font family name.
#' @keywords internal
resolve_font <- function(family, fallback = "sans") {
  # look up all installed families via systemfonts (no extrafont import step)
  installed <- unique(systemfonts::system_fonts()$family)

  if (family %in% installed) {
    return(family)
  }

  # warn once per session rather than on every plot
  warn_key <- paste0("ziba_font_warned_", family)
  if (!isTRUE(getOption(warn_key))) {
    warning(
      sprintf("Font '%s' not found; falling back to '%s'.", family, fallback),
      call. = FALSE
    )
    options(stats::setNames(list(TRUE), warn_key))
  }

  fallback
}

#' Ziba Theme
#'
#' Good looking, based on principles from 'Storytelling with data'. Colours
#' come from the session store (see `set_ziba_colours()`), and `colours`
#' overrides them for a single plot.
#'
#' @param base_size Base font size in pts; all other text and spacing scales
#'   from this.
#' @param heading_scale Multiplier on the title and subtitle sizes only. Use
#'   values below 1 for charts placed in documents, where headings would
#'   otherwise overpower the body text.
#' @param font_family Font to use in the graphs. Defaults to the
#'   `ziba.font_family` option, or "Matter" if unset.
#' @param colours Optional named vector or list of colour overrides for this
#'   theme only, e.g. `c(title = "red")`. Names must match `ziba_colours()`.
#'
#' @export
#'
#' @return A ggplot2 theme.
theme_ziba <- function(base_size = 11.5,
                       heading_scale = 1,
                       font_family = getOption("ziba.font_family", "Matter"),
                       colours = NULL) {
  font_family <- resolve_font(font_family)

  # spacing designed at the 11.5pt default, scaled so smaller themes keep
  # the same proportions
  sp <- \(pt) pt * base_size / 11.5

  # start from the session colours, then apply any per-call overrides
  cols <- ziba_colours()
  if (!is.null(colours)) {
    colours <- unlist(colours)
    check_colours(colours)
    cols[names(colours)] <- colours
  }

  # structural theme: layout, sizes, and visibility, but no colours
  structure <- ggplot2::theme_grey(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      # add padding to the plot, scaled to base_size so exports shrink in proportion;
      # top is slightly larger to stop the title clipping in viewers
      plot.margin = ggplot2::margin(
        base_size * 1.2, base_size * 1.5, base_size, base_size, "pt"
      ),

      # remove the panel background and border
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),

      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent", colour = NA),

      # legend sits close under the axis, items on one line with tight spacing
      legend.position = "bottom",
      legend.box.spacing = ggplot2::unit(sp(4), "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.key.spacing.x = ggplot2::unit(sp(10), "pt"),
      legend.key.spacing.y = ggplot2::unit(sp(2), "pt"),

      # add light, dotted major grid lines only
      panel.grid.major.y = ggplot2::element_line(linetype = "dotted", linewidth = 0.2),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      # space between the x axis labels and the panel
      axis.text.x = ggplot2::element_text(
        size = ggplot2::rel(12 / 11.5),
        margin = ggplot2::margin(t = base_size * 0.7)
      ),
      # space between the y axis labels and the panel
      axis.text.y = ggplot2::element_text(
        size = ggplot2::rel(12 / 11.5),
        margin = ggplot2::margin(r = base_size * 0.5)
      ),

      # modify the margins of the title and subtitle
      # sizes are relative to base_size; at 11.5 these match the original pts
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(28 / 11.5 * heading_scale), hjust = 0,
        lineheight = 1,
        margin = ggplot2::margin(l = sp(10), b = sp(6), t = base_size * 0.4, r = sp(10))
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(20 / 11.5 * heading_scale), hjust = 0,
        lineheight = 1,
        margin = ggplot2::margin(l = sp(10), b = sp(20), t = sp(5))
      ),

      # add padding to the caption
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(11 / 11.5), hjust = 1,
        margin = ggplot2::margin(t = sp(15))
      ),

      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(size = ggplot2::rel(16 / 11.5), hjust = 0.95, margin = ggplot2::margin(t = base_size * 0.9)),
      # space between the tick labels and the axis titles
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.9)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.9)),
      # Axis and legend text
      text = ggplot2::element_text(family = font_family),
      axis.text = ggplot2::element_text(size = ggplot2::rel(12 / 11.5), family = font_family),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = ggplot2::rel(12 / 11.5), family = font_family),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(14 / 11.5),
        margin = ggplot2::margin(sp(10), sp(10), sp(10), sp(10), "pt")
      ),

      # make geom_text/geom_label inherit the theme font (ggplot2 >= 4.0)
      geom = ggplot2::element_geom(family = font_family)
    )

  # layer colours on top; + merges, so the structural properties are kept
  structure + do.call(ggplot2::theme, ziba_colour_elements(cols))
}

#' Use the ziba theme globally
#'
#' Sets `theme_ziba()` as the session default, registers it so that later
#' calls to `set_ziba_colours()` update the active theme in place, and makes
#' default discrete colour and fill legends a single row.
#'
#' @param ... Arguments passed to `theme_ziba()`.
#' @return The previous theme, invisibly.
#' @export
use_ziba_theme <- function(...) {
  old <- ggplot2::theme_set(theme_ziba(...))
  .ziba$theme_active <- TRUE


  # one-row legends for default discrete colour and fill scales
  options(
    ggplot2.discrete.colour = ziba_discrete_colour,
    ggplot2.discrete.fill = ziba_discrete_fill
  )


  invisible(old)
}


#' Use the ziba theme globally
#'
#' Alias for `use_ziba_theme()`, kept for older notebooks.
#'
#' @param ... Arguments passed to `theme_ziba()`, e.g. `base_size` and
#'   `heading_scale`.
#' @return The previous theme, invisibly.
#' @export
make_beautiful_plots <- function(...) {
  use_ziba_theme(...)
}


#' Use the ziba theme sized for documents
#'
#' Sets a smaller ziba theme suited to charts placed at text width in PDF or
#' Word documents, where body text is around 11pt.
#'
#' @param base_size Base font size in pts.
#' @param heading_scale Multiplier on the title and subtitle sizes.
#' @param ... Other arguments passed to `theme_ziba()`.
#' @return The previous theme, invisibly.
#' @export
make_document_plots <- function(base_size = 8, heading_scale = 0.65, ...) {
  use_ziba_theme(base_size = base_size, heading_scale = heading_scale, ...)
}
