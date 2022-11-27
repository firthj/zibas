rm(list = ls())
library(devtools)
load_all()
library(tidyverse)

make_beautiful_plots()
test <- 0
max_cols <- 6

cat <- runif(max_cols, 1, 3) %>%
  set_names(letters[1:max_cols])

test <- test + 1

map_df(letters[1:max_cols], ~ {
  exp <- cat[[.x]]
  tibble(x = 1:10, y = x^exp, z = .x)
}) %>%
  ggplot(aes(x, y, colour = z)) +
  geom_line() +
  labs(title = glue("Test {test}"), subtitle = "A visual test") +
  theme_ziba() +
  set_legend_rows(1)
