# make this a function!

#ts_plot <- function(df, rows = 7, cols = 5, npages) {
rows <- 7
cols <- 5
npages <- 3
#a_res <- df

t_res <- rstan::summary(out1, pars="theta", probs=c(.1, .9)) %>%
  first() %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  as_tibble() %>%
  mutate(year = str_extract(parameter, "\\d+(?=,)") %>% 
           as.numeric() %>% 
           factor(labels = min(gm_data$data$year):max(gm_data$data$year)) %>% 
           as.character() %>% 
           as.numeric(),
         country = str_extract(parameter, "(?<=,)\\d+") %>% 
           as.numeric() %>% 
           factor(labels = sort(unique(gm_data$data$kk))) %>% 
           as.character(),
         kk = as.numeric(str_extract(parameter, "(?<=,)\\d+"))) %>% 
  arrange(kk, year)

pages <- c("1:35", "36:70", "71:105")
t_res1 <- t_res %>%
  group_by(country) %>%
  mutate(last_est = last(mean)) %>%
  ungroup()
for (i in 1:npages) {
  cpage <- unique(t_res1$country)[((i-1)*rows*cols)+1:i*rows*cols]
  cpage <- unique(t_res1$country)[c(eval(parse(text=pages[i])))]
  cp <- t_res1[t_res1$country %in% cpage, ]
  cp$country <- factor(cp$country, levels = cpage)

  plotx <- ggplot(data=cp, aes(x=year, y=mean)) +
    geom_line() + theme_bw() +
    theme(legend.position="none") +
    coord_cartesian(xlim=c(1973,2017)) +
    labs(x = NULL, y = "Tolerance") +
    geom_ribbon(aes(ymin = `10%`, ymax = `90%`, linetype=NA), alpha = .25) +
    facet_wrap(~country, ncol = 5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = "white", colour = "white"))

  pdf(file=paste0("paper/figures/ts",i,".pdf"), width=6, height = 9)
  plot(plotx)
  graphics.off()
}




