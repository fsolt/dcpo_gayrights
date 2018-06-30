#Postprocessing
library(tidyverse)
library(DCPO)

load("~/Documents/Projects/dcpo_gayrights/data/gm_2018-06-25_13:00:53.rda")
gm <- read_csv("data/all_data_gm.csv")

x1 <- rstan::summary(out1)
write_csv(as.data.frame(x1$summary), path="data/x1.csv")
x1_sum <- as.data.frame(x1$summary)     # as.data.frame() preserves rownames
x1_sum$parameter <- rownames(x1_sum)
x1_sum$parameter_type <- gsub("([^[]*).*", "\\1", x1_sum$parameter)
View(x1_sum)

x <- x %>% 
    mutate(prob = y_r/n,
           prob_se = sqrt(prob*(1-prob)/n)) %>% 
    bind_cols(x1_sum %>% 
                  filter(parameter_type=="pred_prob") %>% 
                  transmute(pred_prob = mean, 
                            pred_prob_se = sd)) %>% 
    mutate(diff = prob - pred_prob,
           diff_se = sqrt(prob_se^2+pred_prob_se^2),
           diff_t = abs(diff/diff_se),
           well_predicted = diff_t < 1.96)

qcodes <- x %>% group_by(variable) %>%
  summarize(qcode = first(qcode),
            r_n = n()) %>%
  arrange(qcode)

rcodes <- x %>% group_by(variable_cp) %>%
  summarize(rcode = first(rcode),
            r_n = n()) %>%
  arrange(rcode)

kcodes <- x %>%
  group_by(country) %>%
  summarize(ccode = first(ccode),
            firstyr = first(firstyr),
            lastyr = first(lastyr)) %>%
  ungroup()

ktcodes <- tibble(ccode = rep(1:max(x$ccode), each = max(x$tcode)),
                  tcode = rep(1:max(x$tcode), times = max(x$ccode)),
                  ktcode = (ccode-1)*max(tcode)+tcode) %>%
  left_join(kcodes, by = "ccode") %>%
  mutate(year = min(firstyr) + tcode - 1)

b_res <- x1_sum %>%
  filter(parameter_type=="beta") %>%
  select(parameter, mean, `2.5%`, `97.5%`) %>%
  mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
  left_join(rcodes, by="rcode")

a_res <- x1_sum %>% filter(parameter_type=="alpha") %>%
  select(parameter, mean, `2.5%`, `97.5%`) %>%
  mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
  left_join(rcodes, by="rcode")

t_res <- rstan::summary(out1, pars="theta", probs=c(.1, .9)) %>%
  first() %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  as_tibble() %>%
  mutate(ktcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
  left_join(ktcodes, by="ktcode") %>%
  arrange(ccode, tcode)

gm_laws <- read_csv("data-raw/gm_laws.csv", col_types = "ciiii") %>%
    right_join(kcodes, by = "country")

t_res1 <- t_res %>%
  left_join(gm_laws, by = c("ccode", "country", "firstyr", "lastyr")) %>%
  transmute(country = country,
            term = country,
            kk = ccode,
            year = year,
            estimate = mean,
            lb = `10%`,
            ub = `90%`,
            law = ifelse(!is.na(gm) & (year >= gm | year>=lastyr), "Marriage",
                         ifelse(!is.na(civ) & (year >= civ | year>=lastyr), "Civil Union",
                                "None"))) %>%
  arrange(kk, year)

# Plots:
#   1. tolerance by country, most recent available year: cs_plot
#   2. tolerance trends, estimate plus raw data, eight countries
#   3. trends in all countries: ts_plot
#   4. probability of tolerant answer by tolerance (alpha and beta), selected items (modelled on McGann2014, fig 1)
#   5. bar chart of alpha and beta for all items?
