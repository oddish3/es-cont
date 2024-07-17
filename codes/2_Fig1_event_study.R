# Replication files for Event Studies with a Continuous Treatment
# by Brantly Callaway, Andrew Goodman-Bacon and Pedro H. C. Sant'Anna
#-----------------------------------------------------------------------------
# Empirical application based on Bartik, Currie, Greenstone, Knittel (2019, AEJ)
#-----------------------------------------------------------------------------
# This portion of the codes replicates the event studies based on ATT^* aggregations
#-----------------------------------------------------------------------------
# Clear memory
rm(list=ls())
# Load packages
library(tidyr)
library(dplyr)
library(fixest)
library(broom)
library(stringr)
library(here)
library(ggplot2)
library(ggthemes)
library(did)
set.seed(20240103)
#---------------------------------------------------------------------------------------
# Load data
load(here("data/processed/data_BCGK2019.RData"))
# Load ggplot theme
source(here("codes/00_ggplot_theme.R"))
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Event-study plots using did packaged, for above median
att_above <- did::att_gt(yname = "y",
                         tname = "t",
                         idname = "i",
                         gname = "G",
                         data = data_bcgk_above_median,
                         control_group = "notyettreated",
                         base_period = "universal",
                         est_method = "reg")

es_above <- did::aggte(att_above,
                       type = "dynamic",
                       min_e = -11,
                       max_e = 4)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Event-study plots using did packaged, for below median
att_below <- did::att_gt(yname = "y",
                         tname = "t",
                         idname = "i",
                         gname = "G",
                         data = data_bcgk_below_median,
                         control_group = "notyettreated",
                         base_period = "universal",
                         est_method = "reg")

es_below <- did::aggte(att_below,
                       type = "dynamic",
                       min_e = -11,
                       max_e = 4)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Put the coefficients in a tibble that is easy to plot
es_above_tibble <- broom::tidy(es_above) %>% 
  mutate(estimate = ifelse(is.na(estimate), 0, estimate),
         conf.low = ifelse(is.na(conf.low), 0, conf.low),
         conf.high = ifelse(is.na(conf.high), 0, conf.high),
         p_conf_low = ifelse(is.na(point.conf.low), 0, point.conf.low),
         p_conf_high = ifelse(is.na(point.conf.high), 0, point.conf.high))
es_below_tibble <- broom::tidy(es_below) %>% 
  mutate(estimate = ifelse(is.na(estimate), 0, estimate),
         conf.low = ifelse(is.na(conf.low), 0, conf.low),
         conf.high = ifelse(is.na(conf.high), 0, conf.high),
         p_conf_low = ifelse(is.na(point.conf.low), 0, point.conf.low),
         p_conf_high = ifelse(is.na(point.conf.high), 0, point.conf.high))

range(es_above_tibble$conf.low, es_above_tibble$conf.high,
      es_below_tibble$conf.low, es_below_tibble$conf.high, na.rm = TRUE)
#---------------------------------------------------------------------------------------
# Plots for ES-above-median data
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
p_es_above<- ggplot(data = es_above_tibble,
                  mapping = aes(x = event.time, y = estimate)) +
  geom_line(linewidth = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = 'grey', linewidth = 1., linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, linewidth = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = p_conf_low, ymax = p_conf_high), show.legend = FALSE, linewidth = 1.)+
  xlab("Years relative to first shale drilling") +
  ylab("Average differences in log(employment)") +
  ylim(range(-0.04, 0.12))+
  scale_y_continuous(breaks = seq(-0.04, .12, 0.02), limits = c(-0.04,0.12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )  

p_es_above
#---------------------------------------------------------------------------------------
# # Save the plot
# ggsave(here("plots/event_study_above.pdf"),
#        plot = p_es_above, width = 12, height = 5,  bg = "transparent", dpi = 300)

#---------------------------------------------------------------------------------------
# Plots for ES-below-median data
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
p_es_below<- ggplot(data = es_below_tibble,
                    mapping = aes(x = event.time, y = estimate)) +
  geom_line(linewidth = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = 'grey', linewidth = 1., linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, linewidth = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = p_conf_low, ymax = p_conf_high), show.legend = FALSE, linewidth = 1.)+
  xlab("Years relative to first shale drilling") +
  ylab("Average differences in log(employment)") +
  #ylim(range(-0.04, 0.12))+
  scale_y_continuous(breaks = seq(-0.04, .12, 0.02), limits = c(-0.04,0.12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )  

p_es_below
#---------------------------------------------------------------------------------------
# # Save the plot
# ggsave(here("plots/event_study_below.pdf"),
#        plot = p_es_below, width = 12, height = 5,  bg = "transparent", dpi = 300)
#---------------------------------------------------------------------------------------
# Put these two ES plots together (Figure 1 in the paper)
p_es_together<- ggplot(data = es_above_tibble,
                    mapping = aes(x = event.time, y = estimate)) +
  #geom_line(linewidth = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = 'grey', linewidth = 1., linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  geom_ribbon(aes(ymin= p_conf_low, ymax=  p_conf_high),
              alpha = .1,
              size = .3,
              fill = "darkblue")+
  # geom_ribbon(aes(ymin= conf.low, ymax=  conf.high),
  #             alpha = .1,
  #             size = .3,
  #             fill = "darkblue")+
  geom_line(linewidth = 1.2,
            alpha = 2,
            colour = "darkblue") +
  geom_ribbon(data = es_below_tibble,aes(ymin= p_conf_low, ymax=  p_conf_high),
              alpha = .2,
              size = 1,
              fill = "#E69F00")+
  # geom_ribbon(data = es_below_tibble,aes(ymin= conf.low, ymax=  conf.high),
  #             alpha = .1,
  #             size = 1,
  #             fill = "#E69F00")+
  geom_line(data = es_below_tibble,linewidth = 1.2,
            alpha = 2,
            colour = "#E69F00") +

  xlab("Years relative to first shale drilling") +
  ylab("Average differences in log(employment)") +

  scale_y_continuous(breaks = seq(-0.04, .12, 0.02), limits = c(-0.04,0.12))+
  scale_x_continuous(breaks = seq(-11,4, 1))+
  theme(axis.text.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.title = element_text(color="black",
                                  size = 20))+
  theme(plot.title=ggtext::element_markdown(size=20,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )  

p_es_together
#---------------------------------------------------------------------------------------
# Save the plot
ggsave(here("plots/Fig1.pdf"),
       plot = p_es_together, width = 12, height = 5,  bg = "transparent", dpi = 300)
#---------------------------------------------------------------------------------------
# Appendix Figure for event study across all doses
# Event-study plots using did packaged, for all units

att_all <- did::att_gt(yname = "y",
                       tname = "t",
                       idname = "i",
                       gname = "G",
                       data = data_bcgk %>% mutate(G = ifelse(d==0, Inf, G)),
                       control_group = "notyettreated",
                       base_period = "universal",
                       est_method = "reg")

es_all <- did::aggte(att_all,
                     type = "dynamic",
                     min_e = -11,
                     max_e = 4)
# Put the coefficients in a tibble that is easy to plot
es_all_tibble <- broom::tidy(es_all) %>% 
  mutate(estimate = ifelse(is.na(estimate), 0, estimate),
         conf.low = ifelse(is.na(conf.low), 0, conf.low),
         conf.high = ifelse(is.na(conf.high), 0, conf.high),
         p_conf_low = ifelse(is.na(point.conf.low), 0, point.conf.low),
         p_conf_high = ifelse(is.na(point.conf.high), 0, point.conf.high))

p_es_all<- ggplot(data = es_all_tibble,
                  mapping = aes(x = event.time, y = estimate)) +
  geom_line(linewidth = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = 'grey', linewidth = 1., linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, linewidth = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = p_conf_low, ymax = p_conf_high), show.legend = FALSE, linewidth = 1.)+
  xlab("Years relative to first shale drilling") +
  ylab("Average differences in log(employment)") +
  scale_y_continuous(breaks = seq(-0.04, .12, 0.02), limits = c(-0.04,0.12))+
  scale_x_continuous(breaks = seq(-11,4, 1))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )  

p_es_all
#---------------------------------------------------------------------------------------
#ES with all units but using the style of the paper
p_es_all2 <- ggplot(data = es_all_tibble,
                   mapping = aes(x = event.time, y = estimate)) +
  #geom_line(linewidth = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = 'grey', linewidth = 1., linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  geom_ribbon(aes(ymin= p_conf_low, ymax=  p_conf_high),
              alpha = .1,
              size = .3,
              fill = "#009E73")+
  # geom_ribbon(aes(ymin= conf.low, ymax=  conf.high),
  #             alpha = .1,
  #             size = .3,
  #             fill = "#009E73")+
  geom_line(linewidth = 1.2,
            alpha = 2,
            colour = "#009E73") +
  xlab("Years relative to first shale drilling") +
  ylab("Average differences in log(employment)") +
  
  scale_y_continuous(breaks = seq(-0.04, .12, 0.02), limits = c(-0.04,0.12))+
  scale_x_continuous(breaks = seq(-11,4, 1))+
  theme(axis.text.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.title = element_text(color="black",
                                  size = 20))+
  theme(plot.title=ggtext::element_markdown(size=20,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )  

p_es_all2
#---------------------------------------------------------------------------------------
# Save the plot
ggsave(here("plots/Appendix_Fig_ES.pdf"),
       plot = p_es_all2, width = 12, height = 5,  bg = "transparent", dpi = 300)
