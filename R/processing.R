library(tidyverse)
library(foreign)
library(haven)
library(reshape2)
library(beepr)
library(rstan)
library(stringr)
library(DCPO)

gm <- dcpo_setup(vars = "data-raw/surveys_gm.csv",
                 file = "results/all_data_gm.csv")

gm <- read_csv("results/all_data_gm.csv")

x <- gm %>% with_min_yrs(3)
out1 <- dcpo(x, iter = 4000)
save(out1, file = str_c("results/gm_", str_replace(Sys.time(), " ", "_"), ".rda"))


ab <- dcpo_setup(vars = "data-raw/surveys_abortion.csv",
                 file = "results/all_data_abortion.csv")

x <- ab %>% with_min_yrs(3)
out1 <- dcpo(x)
save(out1, file = str_c("results/ab_", str_replace(Sys.time(), " ", "_"), ".rda"))
