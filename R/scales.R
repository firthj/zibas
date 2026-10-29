# Divisors with a default suffix, matched numerically so the lookup does not
# depend on how R formats numbers (e.g. the scipen option)
.unit_divisors <- c(1, 1e3, 1e6, 1e9)
.unit_suffixes <- c("", "k", "m", "bn")


# Look up the suffix for a scale divisor, or use the caller's suffix if given
unit_suffix <- function(scale, suffix = NULL) {
  if (!is.numeric(scale) || length(scale) != 1 || is.na(scale) || scale <= 0) {
    stop("`scale` must be a single positive number, e.g. 1e6.", call. = FALSE)
  }

  if (!is.null(suffix)) {
    return(suffix)
  }

  # compare with a relative tolerance to absorb floating-point error
  hit <- which(abs(.unit_divisors - scale) <= 1e-9 * scale)
  if (length(hit) == 0) {
    stop(
      sprintf(
        "No default suffix for scale = %s. Use 1, 1e3, 1e6 or 1e9, or supply `suffix`.",
        format(scale)
      ),
      call. = FALSE
    )
  }

  .unit_suffixes[[hit]]
}


# Build a number labeller that divides by `scale` and appends the unit suffix
number_labeller <- function(scale = 1, prefix = "", suffix = NULL,
                            big.mark = " ", accuracy = NULL) {
  scales::label_number(
    accuracy = accuracy,
    scale = 1 / scale,
    prefix = prefix,
    suffix = unit_suffix(scale, suffix),
    big.mark = big.mark
  )
}


# Pick the continuous scale function for an axis
axis_scale <- function(axis) {
  switch(axis,
    x = ggplot2::scale_x_continuous,
    y = ggplot2::scale_y_continuous
  )
}


#' Easy number scales
#'
#' Continuous axis scales that format numbers with a thousands separator and
#' shorten them to metric units. For example, `scale_y_number(1e6)` shows
#' 2 500 000 as "2.5M".
#'
#' @name num_scale
#' @param scale Divisor for the axis values. 1, 1e3, 1e6 and 1e9 get the
#'   suffixes "", "k", "M" and "B". Any other positive number needs `suffix`.
#' @param suffix Text to append to each label. If NULL, the function picks it
#'   from `scale`. Use this for other units, e.g. `suffix = " MW"`.
#' @param prefix Text to put before each label. The dollar scales default
#'   to "$".
#' @param big.mark Thousands separator. Defaults to a space, the SI style.
#' @param accuracy Rounding for the labels, e.g. 0.1 for one decimal place.
#'   If NULL, the function chooses a sensible accuracy from the breaks.
#' @param ... Other arguments passed to `ggplot2::scale_x_continuous()` or
#'   `ggplot2::scale_y_continuous()`, such as `limits` or `expand`.
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' df <- data.frame(year = 2025:2030, cost = c(1.2, 1.8, 2.6, 3.1, 4.4, 5.0) * 1e9)
#'
#' ggplot(df, aes(year, cost)) +
#'   geom_line() +
#'   scale_y_dollar(1e9)
#'
#' ggplot(df, aes(year, cost / 1e6)) +
#'   geom_line() +
#'   scale_y_number(suffix = " MW")
NULL


#' @rdname num_scale
#' @export
scale_y_number <- function(scale = 1, ..., suffix = NULL, prefix = "",
                           big.mark = " ", accuracy = NULL) {
  axis_scale("y")(
    ...,
    labels = number_labeller(scale, prefix, suffix, big.mark, accuracy)
  )
}


#' @rdname num_scale
#' @export
scale_x_number <- function(scale = 1, ..., suffix = NULL, prefix = "",
                           big.mark = " ", accuracy = NULL) {
  axis_scale("x")(
    ...,
    labels = number_labeller(scale, prefix, suffix, big.mark, accuracy)
  )
}


#' @rdname num_scale
#' @export
scale_y_dollar <- function(scale = 1, ..., suffix = NULL, prefix = "$",
                           big.mark = " ", accuracy = NULL) {
  scale_y_number(
    scale, ...,
    suffix = suffix, prefix = prefix, big.mark = big.mark, accuracy = accuracy
  )
}


#' @rdname num_scale
#' @export
scale_x_dollar <- function(scale = 1, ..., suffix = NULL, prefix = "$",
                           big.mark = " ", accuracy = NULL) {
  scale_x_number(
    scale, ...,
    suffix = suffix, prefix = prefix, big.mark = big.mark, accuracy = accuracy
  )
}


#' Percentage axis scales
#'
#' Formats proportions as percentages, so 0.25 shows as "25%".
#'
#' @param accuracy Rounding for the labels, e.g. 1 for whole percentages.
#'   If NULL, the function chooses a sensible accuracy from the breaks.
#' @param ... Other arguments passed to `ggplot2::scale_x_continuous()` or
#'   `ggplot2::scale_y_continuous()`.
#' @return A ggplot2 scale.
#' @name percent_scale
NULL


#' @rdname percent_scale
#' @export
scale_y_percent <- function(..., accuracy = NULL) {
  axis_scale("y")(..., labels = scales::label_percent(accuracy = accuracy))
}


#' @rdname percent_scale
#' @export
scale_x_percent <- function(..., accuracy = NULL) {
  axis_scale("x")(..., labels = scales::label_percent(accuracy = accuracy))
}
