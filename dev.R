rm(list = ls())
library(devtools)
load_all()
library(tidyverse)

make_beautiful_plots()

walk(list.files(pattern = "png"), unlink)


mtcars %>%
  ggplot(aes(hp, mpg * 100, fill = mpg)) +
  geom_point() +
  scale_y_dollar(1e3) +
  ggtitle("Plot 1") +
  labs(caption = "Test caption") +
  extend_right(2)



# mtcars %>%
#   ggplot(aes(hp, mpg * 100)) +
#   geom_point() +
#   labs(title = "Plot 2") +
#   scale_y_percent()
