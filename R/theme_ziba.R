#' @importFrom grDevices pdfFonts
.onAttach <- function(libname, pkgname) { # nolint
  ## Load all fonts
  extrafont::loadfonts(quiet = TRUE)
}



#' Ziba Theme
#'
#' Good looking, based on principles from 'Storytelling with data'.
#'
#' @param font_family Font to use in the graphs, defaults to Roboto.
#'
#' @import ggplot2
#'
#' @importFrom grid unit
#'
#' @export
#'
#' @return A ggplot2 theme.
theme_ziba <- function(font_family = "Source Sans Pro") {
  ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = 11.5, base_family = font_family),
    ggplot2::theme(
      # add padding to the plot
      plot.margin = grid::unit(c(0.5, 1.5, 0.5, 0.5), "cm"),

      # remove the plot background and border
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),

      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.position = "bottom",

      # add light, dotted major grid lines only
      panel.grid.major.y = ggplot2::element_line(
        linetype = "dotted",
        colour = "grey60",
        size = 0.2
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "gray60", size = 0.3),

      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(
        size = 26, colour = "gray30",
        hjust = 0,
        margin = ggplot2::margin(b = 10, t = 5)
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        size = 18, colour = "grey50",
        hjust = 0.003,
        margin = ggplot2::margin(b = 10)
      ),


      # add padding to the caption
      plot.caption = ggplot2::element_text(
        size = 11, colour = "grey60",
        hjust = 1,
        margin = ggplot2::margin(t = 15)
      ),

      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(
        size = 16, colour = "gray30",
        hjust = 0.95
      ),
      axis.text = ggplot2::element_text(size = 12, colour = "gray40"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12, colour = "gray40"),
      strip.text = ggplot2::element_text(
        size = 14, colour = "gray40",
        margin = ggplot2::margin(
          10, 10,
          10, 10,
          "pt"
        )
      )
    )
  )
}


#' Ziba theme for ridge plots
#'
#' Ridge plots require a different offset for y axis labels, and look better with lighter grid lines.
#'
#' @param font_family Font to use in the graphs, defaults to Roboto.
#'
#' @import ggplot2
#' @export
#'
#' @return A ggplot2 theme
#' @examples
#' # ADD_EXAMPLES_HERE
theme_ziba_ridges <- function(font_family = "Roboto") {
  ggplot2::`%+replace%`(
    theme_ziba(font_family),
    ggplot2::theme(
      # add light, dotted major grid lines only
      panel.grid.major = ggplot2::element_line(
        linetype = "solid",
        colour = "grey80",
        size = 0.15
      ),

      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(
        size = 26, colour = "#212121",
        hjust = -0.03,
        margin = ggplot2::margin(b = 10, t = 5)
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        size = 18, colour = "grey40",
        hjust = -0.01,
        margin = ggplot2::margin(b = 10)
      ),
      # remove the axis lines
      axis.line = ggplot2::element_blank(),

      # adjust axis to be inline with ridge
      axis.text.y = element_text(vjust = 0, size = 14),

      # Make legend smaller to account for plot detail
      legend.title = ggplot2::element_text(size = 11, colour = "#454545"),
      legend.text = ggplot2::element_text(size = 11, colour = "#454545"),
      strip.text = ggplot2::element_text(
        size = 11, colour = "#454545",
        margin = ggplot2::margin(
          5, 5,
          5, 5,
          "pt"
        )
      )
    )
  )
}
