# Replication files for Event Studies with a Continuous Treatment
# by Brantly Callaway, Andrew Goodman-Bacon and Pedro H. C. Sant'Anna
#-----------------------------------------------------------------------------
# Empirical application based on Bartik, Currie, Greenstone, Knittel (2019, AEJ)
#-----------------------------------------------------------------------------
# This portion of the codes compute our nonparametric estimates
#-----------------------------------------------------------------------------
# Clear memory
rm(list=ls())
# Load packages
library(tidyr)
library(dplyr)
library(fixest)
library(broom)
library(stringr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(here)
library(latex2exp)
library(patchwork)
library(ggtext)
library(splines2)
#---------------------------------------------------------------------------------------
# Load data
load(here("data/processed/data_BCGK2019.RData"))
# Set seed
set.seed(20240103)
#---------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Start setting up ATT procedure
#-----------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Compute cubic splines 
#-----------------------------------------------------------------------------
splines_SR <- fixest::feols(ddY ~ bSpline(d, 
                                          knots = quantile(d,probs = c(0.25, 0.5, 0.75)),
                                          degree = 3,
                                          intercept = TRUE) -1,
                            data = data_bcgk_SR,
                            cluster = ~ i)

splines_LR <- fixest::feols(ddY ~ bSpline(d,
                                          knots = quantile(d,probs = c(0.25, 0.5, 0.75)),
                                          degree = 3,
                                          intercept = TRUE) -1,
                            data = data_bcgk_LR,
                            cluster = ~ i)

splines_all <- fixest::feols(ddY ~ bSpline(d,
                                           knots = quantile(d,probs = c(0.25, 0.5, 0.75)),
                                           degree = 3,
                                           intercept = TRUE) -1,
                             data = data_bcgk_0_4,
                             cluster = ~ i)
#-----------------------------------------------------------------------------
# Compute ATT(d|d) for all values of the dose d
att_SR <- predict(splines_SR)
att_LR <- predict(splines_LR)
att_all <- predict(splines_all)
#-----------------------------------------------------------------------------
# Compute influence functions
#-----------------------------------------------------------------------------
# Get splines for dosage
spline_dosage_SR <- bSpline(data_bcgk_SR$d,
                            knots = quantile(data_bcgk_SR$d,probs = c(0.25, 0.5, 0.75)),
                            degree = 3,
                            intercept = TRUE)

spline_dosage_LR <- bSpline(data_bcgk_LR$d,
                            knots =quantile(data_bcgk_LR$d,probs = c(0.25, 0.5, 0.75)),
                            degree = 3,
                            intercept = TRUE)

spline_dosage_all <- bSpline(data_bcgk_0_4$d,
                             knots =quantile(data_bcgk_0_4$d,probs = c(0.25, 0.5, 0.75)),
                             degree = 3,
                             intercept = TRUE)

# Sample Size
n_treated_SR <- length(data_bcgk_SR$d)
n_treated_LR <- length(data_bcgk_LR$d)
n_treated_all <- length(data_bcgk_0_4$d)

# compute influence function of spline beta
infl_reg_SR <- splines_SR$residuals * spline_dosage_SR %*% (MASS::ginv(t(spline_dosage_SR)%*%spline_dosage_SR/n_treated_SR))
infl_reg_LR <- splines_LR$residuals * spline_dosage_LR %*% (MASS::ginv(t(spline_dosage_LR)%*%spline_dosage_LR/n_treated_LR))
infl_reg_all <- splines_all$residuals * spline_dosage_all %*% (MASS::ginv(t(spline_dosage_all)%*%spline_dosage_all/n_treated_all))
# Now, put all terms together to get the influence function of the ATT(d|d)
infl_att_SR <-  infl_reg_SR %*% t(spline_dosage_SR)
infl_att_LR <-  infl_reg_LR %*% t(spline_dosage_LR)
infl_att_all <-  infl_reg_all %*% t(spline_dosage_all)
# Compute standard error
se_att_SR <- sqrt(colMeans(infl_att_SR^2)/(n_treated_SR))
se_att_LR <- sqrt(colMeans(infl_att_LR^2)/(n_treated_LR))
se_att_all <- sqrt(colMeans(infl_att_all^2)/(n_treated_all))

# Put results in a data frame
results_cdid_SR <- data.frame(d = data_bcgk_SR$d,
                              att = att_SR,
                              p_uci_att = (att_SR + 1.96*se_att_SR),
                              p_lci_att = (att_SR - 1.96*se_att_SR))%>%
  filter(d<6 & d>2)
results_cdid_LR <- data.frame(d = data_bcgk_LR$d,
                              att = att_LR,
                              p_uci_att = (att_LR + 1.96*se_att_LR),
                              p_lci_att = (att_LR - 1.96*se_att_LR) 
)%>%
  filter(d<6 & d>2)
results_cdid_all <- data.frame(d = data_bcgk_0_4$d,
                               att = att_all,
                               p_uci_att = (att_all + 1.96*se_att_all),
                               p_lci_att = (att_all - 1.96*se_att_all))%>%
  filter(d<6 & d>2)

#-----------------------------------------------------------------------------
# #-----------------------------------------------------------------------------
# Save results
save.image(here("data/processed/splines_estimates1.RData"))