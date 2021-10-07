library(tidyverse)
library(DCPOtools)
library(rstan)

gm <- dcpo_setup(vars = "data-raw/surveys_gm.csv",
                 file = "data/all_data_gm.csv")

gm0 <- read_csv("data/all_data_gm.csv", col_types = "cdcddcd")

gm_data <- format_dcpo(gm %>% with_min_yrs(3),
                       scale_q = "marry4a",
                       scale_cp = 2)

start <- proc.time()
out3 <- rstan::stan(file = '../DCPO/inst/stan/dcpo.stan',
                 data = gm_data,
                 iter = 100,
                 chains= 3,
                 cores = 3,
                 # pars = c("sd_delta","sd_theta_evolve","sd_sigma_evolve","sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"),
                 control = list(adapt_delta = 0.99, stepsize = 0.002, max_treedepth = 14))
runtime <- proc.time() - start
runtime
beepr::beep()
save(gm_data, out1, runtime, file = str_c("data/gm_", str_replace(Sys.time(), " ", "_"), ".rda"))


start <- proc.time()
out1 <- dcpo(gm_data, iter = 100)
runtime <- proc.time() - start
runtime
save(x, out1, runtime, file = str_c("data/marry4a_2k", str_replace(Sys.time(), " ", "_"), ".rda"))

x1 <- rstan::summary(out1)
x1_sum <- as.data.frame(x1$summary)     # as.data.frame() preserves rownames
x1_sum$parameter <- rownames(x1_sum)
x1_sum$parameter_type <- gsub("([^[]*).*", "\\1", x1_sum$parameter)

x2 <- x %>% 
    mutate(prob = y_r/n,
           prob_se = sqrt(prob*(1-prob)/n)) %>% 
    bind_cols(x1_sum %>% 
                  filter(parameter_type=="pred_prob") %>% 
                  transmute(pred_prob = mean, 
                            pred_prob_se = sd)) %>% 
    group_by(variable_cp) %>% 
    mutate(vcp_mean = mean(prob)) %>% 
    ungroup() %>% 
    mutate(diff = prob - pred_prob,
           diff_se = sqrt(prob_se^2+pred_prob_se^2),
           diff_t = abs(diff/diff_se),
           well_predicted = diff_t < 1.96,
           diff_vcp_mean = prob - vcp_mean)

r2_by_item <- x2 %>% 
    group_by(variable_cp) %>%
    summarise(rmsr = sqrt(mean(diff^2)),
              rmsr_item_means_only = mean(abs(diff_vcp_mean)),
              r2_by_item = 1 - (sum(diff^2)/sum((prob - mean(prob))^2)),
              prob = mean(prob), 
              pred_prob = mean(pred_prob),
              diff = mean(abs(diff)))

overall_fit <- x2 %>% 
    summarize(rmsr = sqrt(mean(diff^2)),
              rmsr_item_means_only = mean(abs(diff_vcp_mean)),
              by_item_r2 = 1 - (sum(diff^2)/sum(diff_vcp_mean^2)))

p <- x$y_r/x$n

beepr::beep()

shinystan::launch_shinystan(out1)


ab <- dcpo_setup(vars = "data-raw/surveys_abortion.csv",
                 file = "data/all_data_abortion.csv")

ab <- read_csv("data/all_data_abortion.csv", col_types = "cdciiiciiiciiiiiii")

start <- proc.time()
x <- ab %>% with_min_yrs(3)
out1 <- dcpo(x, iter = 8000)
runtime <- proc.time() - start
runtime
save(x, out1, runtime, file = str_c("data/ab_", str_replace(Sys.time(), " ", "_"), ".rda"))

x1 <- rstan::summary(out1)
x1_sum <- as.data.frame(x1$summary)     # as.data.frame() preserves rownames
x1_sum$parameter <- rownames(x1_sum)
x1_sum$parameter_type <- gsub("([^[]*).*", "\\1", x1_sum$parameter)

x2 <- x %>% 
    mutate(prob = y_r/n,
           prob_se = sqrt(prob*(1-prob)/n)) %>% 
    bind_cols(x1_sum %>% 
                  filter(parameter_type=="pred_prob") %>% 
                  transmute(pred_prob = mean, 
                            pred_prob_se = sd)) %>% 
    group_by(variable_cp) %>% 
    mutate(vcp_mean = mean(prob)) %>% 
    ungroup() %>% 
    mutate(diff = prob - pred_prob,
           diff_se = sqrt(prob_se^2+pred_prob_se^2),
           diff_t = abs(diff/diff_se),
           well_predicted = diff_t < 1.96,
           diff_vcp_mean = prob - vcp_mean)

r2_by_item <- x2 %>% 
    group_by(variable_cp) %>%
    summarise(rmsr = sqrt(mean(diff^2)),
              rmsr_item_means_only = mean(abs(diff_vcp_mean)),
              r2_by_item = 1 - (sum(diff^2)/sum((prob - mean(prob))^2)),
              prob = mean(prob), 
              pred_prob = mean(pred_prob),
              diff = mean(abs(diff)))

overall_fit <- x2 %>% 
    summarize(rmsr = sqrt(mean(diff^2)),
              rmsr_item_means_only = mean(abs(diff_vcp_mean)),
              by_item_r2 = 1 - (sum(diff^2)/sum(diff_vcp_mean^2)))

p <- x$y_r/x$n # for shinystan

save(x2, r2_by_item, overall_fit, file = "data/ab_fit.rda")
