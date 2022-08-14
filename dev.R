library(devtools)
load_all()
library(tidyverse)
make_beautiful_plots()
mtcars %>%
  ggplot(aes(hp, mpg)) +
  geom_point() +
  labs(title = "soemthing")
