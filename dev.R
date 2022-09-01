rm(list = ls())
library(devtools)
load_all()
library(tidyverse)
make_beautiful_plots()
mtcars %>%
  ggplot(aes(hp, mpg * 100)) +
  geom_point() +
  labs(title = "soemthing") +
  scale_y_number(1e9)
