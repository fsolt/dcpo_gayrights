# make this a function!
library(forcats)

p1_data <- t_res1 %>%
  group_by(country) %>%
    top_n(1, year) %>%
  ungroup() %>%
  arrange(-estimate) %>%
  mutate(half = ntile(estimate, 2))

p1_data_a <- p1_data %>% filter(half==2) %>% mutate(ranked = as.factor(row_number(estimate)))
p1_data_b <- p1_data %>% filter(half==1) %>% mutate(ranked = as.factor(row_number(estimate)))

p1a <- ggplot(p1_data_a, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = as.factor(ranked)),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="gray75",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None"),
                    name = "Legal Recognition") +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position=c(.237, .9485),
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7),
                     legend.text = element_text(size = 7),
                     legend.title = element_text(size=7, face = "bold"),
                     legend.key.size = unit(.5, "line"),
                     legend.background = element_rect(linetype = "solid",
                                                      color = "grey80",
                                                      size = .25),
                     legend.key = element_rect(colour = "white")) +
  scale_y_discrete(breaks = p1_data_a$ranked, labels=p1_data_a$country) +
  coord_cartesian(xlim=c(-1, 1)) +
  labs(x = "Tolerance", y = NULL)

p1b <- ggplot(p1_data_b, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = ranked),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="grey50",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None")) +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position="none",
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7)) +
  scale_y_discrete(breaks = p1_data_b$ranked, labels=p1_data_b$country) +
  coord_cartesian(xlim=c(-1, 1)) +
  labs(x = "Tolerance", y = NULL)

library(gtable)
library(grid) # low-level grid functions are required

g1 <- ggplotGrob(p1a)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(p1b)
g <- cbind(g1, g2, size="first") # put the two plots side-by-side
g$heights <- unit.pmax(g1$heights, g2$heights) # use the largest height
g$layout[grepl("guide", g$layout$name),c("l","r")] <- c(ncol(g), 1) # center the legend horizontally
grid.newpage()
grid.draw(g)

pdf("paper/figures/cs.pdf")
grid.draw(g)
dev.off()



# With year colors
#
# p1_data <- t_res1 %>% group_by(country) %>% top_n(1, year) %>% ungroup() %>%
#   arrange(-estimate) %>% mutate(half = ntile(estimate, 2),
#                                 ranked = row_number(estimate),
#                                 yr = year)
# p1_data$yr <- car::recode(p1_data$yr, "2001:2010 = 'Before 2011'")
# p1_data$yr <- factor(p1_data$yr, levels = c("Before 2011", "2011", "2012", "2013", "2014", "2015"))
#
# p1_data_a <- p1_data %>% filter(half==2)
#
# p1a <- ggplot(p1_data_a, aes(x = estimate,
#                              y = row_number(estimate), colour=yr)) +
#   scale_fill_manual(values = c("black", "white")) +
#   geom_point(aes(fill = as.factor(civ)), shape = 21, na.rm = TRUE) +
#   theme_bw() + theme(legend.position="none") +
#   geom_segment(aes(x = lb,
#                    xend = ub,
#                    y = row_number(estimate), yend = row_number(estimate),
#                    colour=yr), na.rm = TRUE) +
#   scale_y_discrete(breaks = row_number(p1_data_a$estimate), labels=p1_data_a$country) +
#   coord_cartesian(xlim=c(0, 1)) +
#   ylab("") + xlab("")

p1_data <- t_res1 %>% group_by(country) %>% top_n(1, year) %>% ungroup() %>%
  arrange(-estimate) %>% mutate(quarter = ntile(estimate, 4))

p1_data_a <- p1_data %>% filter(quarter==4) %>% mutate(ranked = row_number(estimate))
p1_data_b <- p1_data %>% filter(quarter==3) %>% mutate(ranked = row_number(estimate))
p1_data_c <- p1_data %>% filter(quarter==2) %>% mutate(ranked = row_number(estimate))
p1_data_d <- p1_data %>% filter(quarter==1) %>% mutate(ranked = row_number(estimate))

p1_data_c$country[p1_data_c$country=="Trinidad and Tobago"] <- "Trinidad"
p1_data_c$country[p1_data_c$country=="Bosnia and Herzegovina"] <- "Bosnia"


p1a <- ggplot(p1_data_a, aes(x = estimate,
                             y = ranked)) +
    geom_segment(aes(x = lb, xend = ub,
                     y = ranked, yend = as.factor(ranked)),
                 na.rm = TRUE) +
    scale_fill_manual(values = c("Marriage"="white",
                                 "Civil Union"="gray75",
                                 "None"="black"),
                      breaks=c("Marriage", "Civil Union", "None"),
                      name = "Legal Recognition") +
    geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
    theme_bw() + theme(legend.position=c(.25, .905),
                       axis.text.x  = element_text(size=7),
                       axis.text.y  = element_text(size=7),
                       axis.title.x = element_text(face="bold", size=7),
                       legend.text = element_text(size = 7),
                       legend.title = element_text(size=7, face = "bold"),
                       legend.key.size = unit(.5, "line"),
                       legend.background = element_rect(linetype = "solid",
                                                        color = "grey80",
                                                        size = .25),
                       legend.key = element_rect(colour = "white")) +
    scale_y_discrete(breaks = p1_data_a$ranked, labels=p1_data_a$country) +
    coord_cartesian(xlim=c(-1, 1)) +
    labs(x = "Tolerance", y = NULL)
ggsave("paper/figures/cs4-1.pdf", height = 4, width = 3)

p1b <- ggplot(p1_data_b, aes(x = estimate,
                             y = fct_reorder(as.factor(country), ranked))) +
    geom_segment(aes(x = lb, xend = ub,
                     y = fct_reorder(as.factor(country), ranked), yend = fct_reorder(as.factor(country), ranked)),
                 na.rm = TRUE) +
    scale_fill_manual(values = c("Marriage"="white",
                                 "Civil Union"="grey50",
                                 "None"="black"),
                      breaks=c("Marriage", "Civil Union", "None")) +
    geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
    theme_bw() + theme(legend.position="none",
                       axis.text.x  = element_text(size=7),
                       axis.text.y  = element_text(size=7),
                       axis.title.x = element_text(face="bold", size=7)) +
    coord_cartesian(xlim=c(-1, 1)) +
    labs(x = "Tolerance", y = NULL)
ggsave("paper/figures/cs4-2.pdf", height = 4, width = 3)

p1c <- ggplot(p1_data_c, aes(x = estimate,
                             y = fct_reorder(as.factor(country), ranked))) +
    geom_segment(aes(x = lb, xend = ub,
                     y = fct_reorder(as.factor(country), ranked), yend = fct_reorder(as.factor(country), ranked)),
                 na.rm = TRUE) +
    scale_fill_manual(values = c("Marriage"="white",
                                 "Civil Union"="grey50",
                                 "None"="black"),
                      breaks=c("Marriage", "Civil Union", "None")) +
    geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
    theme_bw() + theme(legend.position="none",
                       axis.text.x  = element_text(size=7),
                       axis.text.y  = element_text(size=7),
                       axis.title.x = element_text(face="bold", size=7)) +
    coord_cartesian(xlim=c(-1, 1)) +
    labs(x = "Tolerance", y = NULL)
ggsave("paper/figures/cs4-3.pdf", height = 6, width = 3)


p1d <- ggplot(p1_data_d, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = ranked),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="grey50",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None")) +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position="none",
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7)) +
  scale_y_discrete(breaks = p1_data_d$ranked, labels=p1_data_d$country) +
  coord_cartesian(xlim=c(0, 1)) +
  labs(x = "Tolerance", y = NULL)

pdf("paper/figures/cs_top.pdf", height = 3.5, width = 7)
align_plots(p1a, p1b)
dev.off()


pdf("paper/figures/cs_bottom.pdf", height = 3.5, width = 7)
align_plots(p1c, p1d)
dev.off()
