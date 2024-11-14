library(tidyverse)
library(rvest)
library(countrycode)
library(haven)
load("~/Documents/GitHub/dcpo_gayrights/data/theta_summary.rda")
dcpo_input_raw <- read.csv("data/dcpo_input_raw.csv")

gayright_index <- theta_summary %>%
    select(country, year, mean) %>%
    rename(gayright_index = mean)

write_csv(gayright_index, here::here("data","gayright_index.csv"))
write_dta(gayright_index, here::here("data","gayright_index.dta"))
