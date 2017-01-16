gmlc <- gm_laws %>%
  filter(!is.na(civ)) %>%
  arrange(civ) %>%
  mutate(country2 = factor(country, levels = country[order(civ)])) %>%
  filter(civ<=2009)

gmlc_res <- a_res %>%
  filter(country %in% gmlc$country) %>%
  mutate(country2 = factor(country, levels = gmlc$country[order(gmlc$civ)])) %>%
  arrange(country2)

ggplot(data = gmlc_res, aes(x = year, y = estimate)) +
   geom_vline(data = gmlc, aes(xintercept = civ), colour = "#FF9300", linetype = 2) +
   geom_vline(data = gmlc, aes(xintercept = gm), colour = "#011993") +
  theme_bw() +
  theme(legend.position="none") +
  coord_cartesian(xlim = c(1975, 2016), ylim = c(0, 1)) +
  labs(x = NULL, y = "Tolerance") +
  geom_ribbon(aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
  geom_line() +
  facet_wrap(~country2, nrow = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white", colour = "white"))
ggsave("paper/figures/gmlc_ts.pdf", width = 6, height = 4)
