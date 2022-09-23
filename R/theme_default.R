library(tidyverse)
tibble(x = 0:10, y = x^2) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  ggtitle("Plot 1") +
  labs(caption = "Test caption")
