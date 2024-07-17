# Replication files for Event Studies with a Continuous Treatment
# by Brantly Callaway, Andrew Goodman-Bacon and Pedro H. C. Sant'Anna
#-----------------------------------------------------------------------------
# Empirical application based on Bartik, Currie, Greenstone, Knittel (2019, AEJ)
#-----------------------------------------------------------------------------
# This portion of the codes is for data preparation
#-----------------------------------------------------------------------------
# Startup - clear memory and load packages
# Clear memory
rm(list=ls())
#-----------------------------------------------------------------------------
library(haven)
library(tidyr)
library(dplyr)
library(here)
library(BMisc)
library(foreach)
#-----------------------------------------------------------------------------
# Prepare the data
#-----------------------------------------------------------------------------
# Load the data
data_bcgk <- read_dta(here("data/processed/bcgk_replication.dta"))
# sort by i and t (county and year)
data_bcgk <- data_bcgk[order(data_bcgk$i, data_bcgk$t),]
#-----------------------------------------------------------------------------
# Compute median of positive dosages
median_d <- median(data_bcgk$d[data_bcgk$d>0])
# Remove observations with t=2015 as all the data is NA,
# and remove firstG and e as they are not used in the analysis
# and create dummy for above and below median of d among treated
data_bcgk <- data_bcgk %>% 
  filter(t != 2015) %>%
  mutate(d_above_treated_median = 1*(d > median_d)) %>%
  mutate(d_below_treated_median = 1*(d <= median_d)*(d>0)) %>%
  filter(!is.na(y))%>%
  mutate(i = as.numeric(i))
#-----------------------------------------------------------------------------
# Only the balanced panel data component
data_bcgk <- BMisc::makeBalancedPanel(as.data.frame(data_bcgk),
                                      "i",
                                      "t")
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Create subsets of the data
#-----------------------------------------------------------------------------
# Above median treated or never treated (to be used in did package)
data_bcgk_above_median <- data_bcgk %>% 
  filter(d_above_treated_median==1 | d==0)  %>%
  mutate(G = ifelse(d==0, Inf, G)) 

# Below median treated or never treated (to be used in did package)
data_bcgk_below_median <- data_bcgk %>% 
  filter(d_below_treated_median==1 | d==0) %>%
  mutate(G = ifelse(d==0, Inf, G)) 
#-----------------------------------------------------------------------------
# Aggregation based on Short run and Long Run
#-----------------------------------------------------------------------------
# Function that creates the data to be used in dose-response function

data_pre_dose <- function(g,e1=0, e2= 2){
  g_length <- length(g)
  dt1 <- foreach(j=1:g_length) %do% {
    data_bcgk %>%
      mutate(G_cs = ifelse(d==0, Inf, G),
             event_time_cs = t - G_cs)%>%
      filter(G_cs==g[j] | d==0,
             G+e2 <= max(t) | G == Inf) %>%
      mutate(pre_period = g[j] - 1,
             max_post = g[j] +e2) %>%
      filter(t <= max_post & t>= pre_period) %>%
      group_by(i) %>%
      mutate(dY = y - y[t==pre_period]) %>%
      filter(t>pre_period)%>%
      group_by(t) %>%
      mutate(mean_dY_NYT = mean(dY[G_cs>t])) %>%
      mutate(ddY = dY - mean_dY_NYT) %>%
      filter(G_cs==g[j]) %>%
      filter(event_time_cs >= e1 & event_time_cs <= e2) %>%
      group_by(i,G_cs) %>%
      summarise(ddY = mean(ddY),
                d = mean(d),
                ,.groups = "drop_last") %>%
      rename(G = G_cs)
  }
  do.call(rbind,dt1)
}


data_bcgk_SR = data_pre_dose(g = unique(data_bcgk$G),e1=0, e2 = 2)
data_bcgk_LR = data_pre_dose(g = unique(data_bcgk$G),e1=3, e2 = 4)
data_bcgk_0_4 = data_pre_dose(g = unique(data_bcgk$G),e1=0, e2 = 4)
#-----------------------------------------------------------------------------
# compute dose quantiles to avoid extrapolation
quantiles_d <- quantile(data_bcgk$d[data_bcgk$d>0], probs = c(0.05, 0.95))
min_d_pos <- min(data_bcgk$d[data_bcgk$d>0])
max_d_pos <- max(data_bcgk$d[data_bcgk$d>0])
#-----------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Save the data
save.image(here("data/processed/data_BCGK2019.RData"))