load_all()
library(tidyverse)

mtcars %>%
  ggplot(aes(hp, mpg)) +
  geom_point() +
  theme_ziba()