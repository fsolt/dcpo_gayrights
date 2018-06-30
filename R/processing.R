library(tidyverse)
library(DCPO)

gm <- dcpo_setup(vars = "data-raw/surveys_gm.csv",
                 file = "data/all_data_gm.csv")

gm <- read_csv("data/all_data_gm.csv")

start <- proc.time()
x <- gm %>% with_min_yrs(3)
out1 <- dcpo(x, iter = 8000)
save(x, out1, file = str_c("data/gm_", str_replace(Sys.time(), " ", "_"), ".rda"))
runtime <- proc.time() - start
runtime

ab <- dcpo_setup(vars = "data-raw/surveys_abortion.csv",
                 file = "data/all_data_abortion.csv")

x <- ab %>% with_min_yrs(3)
out1 <- dcpo(x)
save(out1, file = str_c("data/ab_", str_replace(Sys.time(), " ", "_"), ".rda"))
