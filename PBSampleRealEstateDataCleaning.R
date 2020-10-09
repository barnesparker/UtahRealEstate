library(tidyverse)
library(DataExplorer)

properties <- read_csv("data/SampleRealEstateRaw.csv")

plot_missing(properties)

properties %>% select_at(vars(-sum(is.na(.) > 0)))

mtcars <- as_tibble(mtcars)
no_missing <- properties %>% 
  select(where(~sum(is.na(.x)) == 0))
