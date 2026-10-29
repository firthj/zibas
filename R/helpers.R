#' Use the ziba theme globally
#'
#' Alias for `use_ziba_theme()`, kept for older notebooks.
#'
#' @param ... Arguments passed to `theme_ziba()`.
#' @return The previous theme, invisibly.
#' @export
make_beautiful_plots <- function(...) {
  use_ziba_theme(...)
}


#' Extend the right plot margin
#'
#' Widens the right margin to make room for direct labels at line ends, the
#' approach 'Storytelling with data' recommends instead of a legend. Pair with
#' `no_clip()` so labels can draw into the margin. Only the right margin
#' changes; the theme's other margins are kept.
#'
#' @param cm Right margin width in cm.
#' @param caption_to_edge If TRUE, a negative right margin pushes the caption
#'   into the extended margin, keeping it flush with the outer edge.
#' @param caption_top Space above the caption in pts; matches `theme_ziba()`.
#' @return A ggplot2 theme.
#' @export
extend_right <- function(cm = 1.5, caption_to_edge = TRUE, caption_top = 15) {
  # plot.margin accepts a partial unit, so theme_ziba's top room survives
  out <- ggplot2::theme(plot.margin = ggplot2::margin_part(r = cm, unit = "cm"))

  if (caption_to_edge) {
    # element_text needs a full margin object, so convert cm to pt
    # (1 cm = 72 / 2.54 pt) and restate the caption's top margin
    out <- out + ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 1,
        margin = ggplot2::margin(t = caption_top, r = -cm * 72 / 2.54, unit = "pt")
      )
    )
  }

  out
}


#' Turn off panel clipping
#'
#' Lets labels and points draw outside the panel, e.g. into the margin
#' created by `extend_right()`.
#'
#' @param ... Other arguments passed to `ggplot2::coord_cartesian()`, such as
#'   `xlim` or `ylim`.
#' @return A ggplot2 coord.
#' @export
no_clip <- function(...) {
  ggplot2::coord_cartesian(..., clip = "off")
}


#' Flip axes, with grid lines to match
#'
#' Wraps `ggplot2::coord_flip()` and moves the dotted grid lines to the
#' value axis, so horizontal bar charts don't need `rev_major_lines()`.
#'
#' @param ... Other arguments passed to `ggplot2::coord_flip()`.
#' @param clip Should drawing be clipped to the panel? Defaults to "on".
#' @return A list of ggplot2 components.
#' @export
flip <- function(..., clip = "on") {
  list(
    ggplot2::coord_flip(..., clip = clip),
    rev_major_lines()
  )
}


#' Swap major grid lines to the x axis
#'
#' Draws the dotted major grid lines vertically instead of horizontally,
#' using the current ziba grid colour.
#'
#' @return A ggplot2 theme.
#' @export
rev_major_lines <- function() {
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(
      linetype = "dotted",
      colour = ziba_colour("grid"),
      linewidth = 0.2
    ),
    panel.grid.major.y = ggplot2::element_blank()
  )
}


#' Remove axis titles
#'
#' Sets the labels to NULL rather than "", so ggplot2 frees the space
#' instead of drawing an empty title.
#'
#' @return A ggplot2 labels specification.
#' @export
no_axis_labs <- function() {
  ggplot2::labs(x = NULL, y = NULL)
}


#' Hide the legend
#'
#' @return A ggplot2 theme.
#' @export
no_leg <- function() {
  ggplot2::theme(legend.position = "none")
}


#' Set plot labels in one call
#'
#' Arguments left at their default keep ggplot2's existing label; pass NULL
#' to remove a label.
#'
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x,y Axis titles.
#' @param caption Plot caption.
#' @return A ggplot2 labels specification.
#' @export
quick_labs <- function(title = ggplot2::waiver(),
                       subtitle = ggplot2::waiver(),
                       x = ggplot2::waiver(),
                       y = ggplot2::waiver(),
                       caption = ggplot2::waiver()) {
  args <- list(title = title, subtitle = subtitle, x = x, y = y, caption = caption)

  # drop waivers so only labels the caller supplied are set
  args <- args[!vapply(args, inherits, logical(1), what = "waiver")]

  do.call(ggplot2::labs, args)
}


#' Manual colour and fill scales with single-row legends
#'
#' Wraps `scale_colour_manual()` and `scale_fill_manual()`. Explicit scales
#' bypass the global one-row legend default, so these apply it directly.
#'
#' @param colour_vector Colours, ideally named to match the data levels.
#' @param ... Other arguments passed to the manual scale.
#' @param nrow Number of legend rows.
#' @return A ggplot2 scale.
#' @export
set_colours <- function(colour_vector, ..., nrow = 1) {
  ggplot2::scale_colour_manual(
    ...,
    values = colour_vector,
    guide = ggplot2::guide_legend(nrow = nrow)
  )
}


#' @rdname set_colours
#' @export
set_fills <- function(colour_vector, ..., nrow = 1) {
  ggplot2::scale_fill_manual(
    ...,
    values = colour_vector,
    guide = ggplot2::guide_legend(nrow = nrow)
  )
}


#' Set the number of legend rows
#'
#' Alias for `ziba_legend_rows()`; applies to both colour and fill legends.
#'
#' @param rows Number of legend rows.
#' @return A ggplot2 guides specification.
#' @export
set_legend_rows <- function(rows = 1) {
  ziba_legend_rows(rows)
}
