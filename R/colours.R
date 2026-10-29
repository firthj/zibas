# Session store for ziba colours. An environment lets setters modify state
# without <<- and keeps it private to the package namespace.
.ziba <- new.env(parent = emptyenv())

# Default colours: brand palette first, then theme roles.
.ziba_default_colours <- c(
  # brand palette
  blue        = "#023e8a",
  red         = "#d62828",
  green       = "#36AE93",
  gray        = "gray60",
  darkgray    = "gray30",

  # theme roles, used by theme_ziba()
  title       = "gray30",
  subtitle    = "grey50",
  caption     = "grey60",
  axis_title  = "gray30",
  axis_text   = "gray40",
  legend_text = "gray40",
  strip_text  = "gray40",
  grid        = "grey60",
  background  = "white"
)

.ziba$colours <- .ziba_default_colours

# Set to TRUE by use_ziba_theme(), so setters know whether to push changes
# into the active global theme.
.ziba$theme_active <- FALSE


# Check that a single value is a colour R can parse
is_colour <- function(x) {
  is.character(x) && length(x) == 1 &&
    !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}


# Check that every colour is named, has a known name, and parses as a colour
check_colours <- function(cols) {
  if (length(cols) == 0) {
    return(invisible(cols))
  }
  if (is.null(names(cols)) || any(names(cols) == "")) {
    stop("All colours must be named, e.g. title = \"#023e8a\".", call. = FALSE)
  }

  unknown <- setdiff(names(cols), names(.ziba_default_colours))
  if (length(unknown) > 0) {
    stop(
      sprintf(
        "Unknown colour name(s): %s.\nValid names: %s.",
        paste(unknown, collapse = ", "),
        paste(names(.ziba_default_colours), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  bad <- names(cols)[!vapply(cols, is_colour, logical(1))]
  if (length(bad) > 0) {
    stop(sprintf("Invalid colour value for: %s.", paste(bad, collapse = ", ")), call. = FALSE)
  }

  invisible(cols)
}


#' Get ziba colours
#'
#' Returns the current session colours, including any changes made with
#' `set_ziba_colours()`.
#'
#' @param ... Optional colour names to return. If empty, returns all colours.
#' @return A named character vector of colours.
#' @export
#' @examples
#' ziba_colours()
#' ziba_colours("blue", "red")
ziba_colours <- function(...) {
  keys <- c(...)
  if (length(keys) == 0) {
    return(.ziba$colours)
  }
  check_colours(stats::setNames(rep("black", length(keys)), keys))
  .ziba$colours[keys]
}


#' Get a single ziba colour
#'
#' @param name A colour name, e.g. "blue" or "title".
#' @return An unnamed colour string.
#' @export
ziba_colour <- function(name) {
  unname(ziba_colours(name))
}


#' Set ziba colours for the session
#'
#' Updates the session colour store. If `use_ziba_theme()` activated the
#' theme, the change also applies to the active global theme straight away.
#'
#' @param ... Named colours, e.g. `title = "#023e8a"`, or a named vector.
#' @return The previous values of the changed colours, invisibly, so you can
#'   restore them with `set_ziba_colours(old)`.
#' @export
#' @examples
#' old <- set_ziba_colours(title = "#023e8a", subtitle = "grey40")
#' set_ziba_colours(old)
set_ziba_colours <- function(...) {
  new <- c(...)
  check_colours(new)

  old <- .ziba$colours[names(new)]
  .ziba$colours[names(new)] <- new
  refresh_active_theme()

  invisible(old)
}


#' Reset ziba colours to package defaults
#'
#' @return The colours in use before the reset, invisibly.
#' @export
reset_ziba_colours <- function() {
  old <- .ziba$colours
  .ziba$colours <- .ziba_default_colours
  refresh_active_theme()
  invisible(old)
}


#' Set the plot title colour
#'
#' Shortcut for `set_ziba_colours(title = col)`.
#'
#' @param col A colour.
#' @export
set_plot_title_colour <- function(col) {
  set_ziba_colours(title = col)
}


#' Set the plot subtitle colour
#'
#' Shortcut for `set_ziba_colours(subtitle = col)`.
#'
#' @param col A colour.
#' @export
set_plot_subtitle_colour <- function(col) {
  set_ziba_colours(subtitle = col)
}


# Build theme elements that set colour only. theme_ziba() and
# refresh_active_theme() both use this list, so colour mapping lives in
# one place.
ziba_colour_elements <- function(cols) {
  list(
    plot.background    = ggplot2::element_rect(fill = cols[["background"]], colour = NA),
    panel.grid.major.y = ggplot2::element_line(colour = cols[["grid"]]),
    plot.title         = ggplot2::element_text(colour = cols[["title"]]),
    plot.subtitle      = ggplot2::element_text(colour = cols[["subtitle"]]),
    plot.caption       = ggplot2::element_text(colour = cols[["caption"]]),
    axis.title         = ggplot2::element_text(colour = cols[["axis_title"]]),
    axis.text          = ggplot2::element_text(colour = cols[["axis_text"]]),
    legend.text        = ggplot2::element_text(colour = cols[["legend_text"]]),
    strip.text         = ggplot2::element_text(colour = cols[["strip_text"]])
  )
}


# Merge current colours into the global theme. theme_update() merges by
# element, so this changes only colours and keeps other theme tweaks.
refresh_active_theme <- function() {
  if (isTRUE(.ziba$theme_active)) {
    do.call(ggplot2::theme_update, ziba_colour_elements(.ziba$colours))
  }
  invisible(NULL)
}


#' Create a blue gradient
#'
#' Extends RColorBrewer with more than 9 shades of blue. The darkest shade is
#' the current ziba "blue", so it follows `set_ziba_colours(blue = ...)`.
#'
#' @param n Number of shades.
#' @return Character vector of `n` hex colours.
#' @export
ziba_gradient <- function(n) {
  blues <- c(RColorBrewer::brewer.pal(9, "Blues")[3:7], ziba_colour("blue"))
  grDevices::colorRampPalette(blues, bias = 1.5, space = "Lab")(n)
}


# Shared builder for the blue scales. The palette is called at plot build
# time, so the scale uses the blue current at render time.
ziba_blue_scale <- function(aesthetic, ..., continuous = FALSE) {
  if (continuous) {
    return(ggplot2::scale_colour_gradientn(..., colours = ziba_gradient(256), aesthetics = aesthetic))
  }
  ggplot2::discrete_scale(aesthetic, palette = ziba_gradient, ...)
}


#' Blue fill gradient for ggplots
#'
#' @param ... Other arguments passed to the underlying fill scale.
#' @param continuous Whether the data is continuous or discrete.
#' @return A ggplot2 scale.
#' @export
scale_fill_ziba_blue <- function(..., continuous = FALSE) {
  ziba_blue_scale("fill", ..., continuous = continuous)
}


#' Blue colour gradient for ggplots
#'
#' @param ... Other arguments passed to the underlying colour scale.
#' @param continuous Whether the data is continuous or discrete.
#' @return A ggplot2 scale.
#' @export
scale_colour_ziba_blue <- function(..., continuous = FALSE) {
  ziba_blue_scale("colour", ..., continuous = continuous)
}

# Default discrete scales with a one-row legend. ggplot2 calls these through
# the ggplot2.discrete.* options when a plot has no explicit colour/fill scale.
ziba_discrete_colour <- function(...) {
  ggplot2::scale_colour_hue(..., guide = ggplot2::guide_legend(nrow = 1))
}

ziba_discrete_fill <- function(...) {
  ggplot2::scale_fill_hue(..., guide = ggplot2::guide_legend(nrow = 1))
}


#' Force single-row legends
#'
#' Add to a plot that uses an explicit scale (e.g. scale_colour_manual),
#' which the global default does not reach.
#'
#' @param nrow Number of legend rows.
#' @return A ggplot2 guides specification.
#' @export
ziba_legend_rows <- function(nrow = 1) {
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = nrow),
    fill = ggplot2::guide_legend(nrow = nrow)
  )
}


# Backwards-compatible single-colour accessors, now read from the store

#' @rdname ziba_colour
#' @export
zibablue <- function() ziba_colour("blue")

#' @rdname ziba_colour
#' @export
zibared <- function() ziba_colour("red")

#' @rdname ziba_colour
#' @export
zibagreen <- function() ziba_colour("green")

#' @rdname ziba_colour
#' @export
zibagray <- function() ziba_colour("gray")

#' @rdname ziba_colour
#' @export
zibadarkgray <- function() ziba_colour("darkgray")
