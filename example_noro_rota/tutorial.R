## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----<packages---------------------------------------------------------------------------------------------------
# load packages:
options(warn = -1)
library(surveillance)

# to install hhh4addon
# install.packages("devtools")
# library(remotes)
# remotes:::install_github("jbracher/hhh4addon", build_vignettes = TRUE)
library(hhh4addon)


## ----<data, cache=TRUE-------------------------------------------------------------------------------------------
# get the data
data("noroBE")
# if you don't have the hhh4addon packae installed you can use:
# load(
# url("https://github.com/cmmid/hhh4-workshop/raw/main/example_noro_rota/noroBE.Rda")
# )


## ----<description------------------------------------------------------------------------------------------------
# look at data:
# time series:
plot(noroBE, ylim = c(0, 40))
# map:
# as population(noroBE) contains population fractions rather than raw
# population sizes setting population = 100000/<population of Berlin>
# will yield cumulative incidence per 100.000 inhabitants; see ?stsplot_space
plot(noroBE, observed ~ unit, population = 100000/3500000, labels = TRUE)


## ----<neighbourhood, cache=TRUE----------------------------------------------------------------------------------
# check the neighbourhood structure, which is stored in the @neighbourhood slot 
# of the sts object:
noroBE@neighbourhood


## ----fit1, cache=TRUE--------------------------------------------------------------------------------------------
# first define a subset of the data to which to fit (will be used in all model
# fits to ensure comparability of AIC values):
subset_fit <- 6:(nrow(noroBE@observed) - 52)
# we are leaving out the last year and the first 5 observations
# the latter serves to ensure comparability to late model versions

##################################################################
# Model 1:
# control list:
ctrl1 <- list(end = list(f = addSeason2formula(~1, S = 1)), # seasonality in end
              # S = 1 means one pair of sine / cosine waves is included
              ar = list(f = ~ 1), # no seasonality in ar
              family = "NegBin1", # negative binomial (rather than Poisson)
              subset = subset_fit)
# fit model
fit1 <- hhh4(noroBE, ctrl1)
# summary of parameter estimates:
summary(fit1)
# you can set idx2Exp = TRUE to get all estimates on the exp-transformed scale
summary(fit1, idx2Exp = TRUE)
# visually inspect:
plot(fit1, unit = 1:12) # look at all units
# get AIC to compare model fit to more complex models
AIC(fit1)


## ----function_pr, cache=TRUE-------------------------------------------------------------------------------------
# helper function to compute Pearson residuals:
pearson_residuals <- function(hhh4Obj){
  # compute raw residuals:
  response_residuals <- residuals(hhh4Obj, type = "response")
  # compute standard deviations:
  if(class(hhh4Obj) == "hhh4lag"){
    sds <- sqrt(fitted(hhh4Obj) + 
                  fitted(hhh4Obj)^2/hhh4addon:::psi2size.hhh4lag(hhh4Obj))
  }else{
    sds <- sqrt(fitted(hhh4Obj) + 
                  fitted(hhh4Obj)^2/surveillance:::psi2size.hhh4(hhh4Obj))
  }
  # compute Pearson residuals:
  pearson_residuals <- response_residuals/sds
  return(pearson_residuals)
}

pr1 <- pearson_residuals(fit1)
# compute district-wise means and standard deviations:
colMeans(pr1)
apply(pr1, 2, sd)


## ----fit2, cache=TRUE--------------------------------------------------------------------------------------------
##################################################################
# Model 2:
# We use fe(..., unitSpecific = TRUE) to add fixed effects for each unit,
# in this case intercepts (1). Seasonality parameters are still shared
# across districts
ctrl2 <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              ar = list(f = ~ 0 + fe(1, unitSpecific = TRUE)),
              family = "NegBin1",
              subset = subset_fit)
# Note: ~0 is necessary as otherwise one would (implicitly) add two intercepts
# unit-specific dispersion parameters could be added by setting family = "NegBinM"
fit2 <- hhh4(noroBE, ctrl2)
# check parameter estimates:
summary(fit2)
# compute AIC
AIC(fit2)


## ----pr2---------------------------------------------------------------------------------------------------------
# compute Pearson residuals and check their mean and variance:
pr2 <- pearson_residuals(fit2)
colMeans(pr2)
apply(pr1, 2, sd)


## ----fit3, cache=TRUE--------------------------------------------------------------------------------------------
# The default setting for the ne component is to use weights 
# neighbourhood(stsObj) == 1 (see ?hhh4).
neighbourhood(noroBE) == 1
# this is because historically neighbourhood matrices were just binary
# however, the path distances are coded in a way that direct neighbours have 
# distance 1, meaning that there is no need to modify the neighbourhood matrix

##################################################################
# Model 3:
ctrl3 <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              ar = list(f = ~ 0 + fe(1, unitSpecific = TRUE)),
              ne = list(f = ~ 0 + fe(1, unitSpecific = TRUE), normalize = TRUE), 
              #  now added ne component to reflect cross-district dependencies
              family = "NegBin1",
              subset = subset_fit)
# normalize = TRUE normalizes weights by the number of neighbours of the 
# exporting district
fit3 <- hhh4(noroBE, ctrl3)
AIC(fit3)
# alternative: use update
# fit3 <- update(fit2, ne = list(f = ~ 0 + fe(1, unitSpecific = TRUE),
#                                weights = neighbourhood(noroBE) == 1,  # little bug?
#                                normalize = TRUE))
summary(fit3) # parameters for different districts are quite different
AIC(fit3)


## ----plot3-------------------------------------------------------------------------------------------------------
plot(fit3, unit = 1:12)


## ----fit4, cache=TRUE--------------------------------------------------------------------------------------------
##################################################################
# Model 4

# For the next model version we will formally include the autoregressive into
# the neighbourhood component
# (i.e. do no longer treat autoregression on the same district separately). 
# This can be done as follows.
# First we need to adapt the neighbourhood matrix, shifting by one
noroBE_power <- noroBE
noroBE_power@neighbourhood <- noroBE@neighbourhood + 1

# new control argument:
ctrl4 <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              # note: line ar = ... is removed
              ne = list(f = ~0 + fe(1, unitSpecific = TRUE),
                        weights = W_powerlaw(maxlag=5, normalize = TRUE,
                                             log = TRUE)), # this is new
              family = "NegBin1",
              subset = subset_fit)
# normalize = TRUE normalizes weights by the number of neighbours of the
# exporting district
# log = TRUE means optimization will be done on a  log scale, ensuring 
# positivity of the decay parameter (which is desirable)
fit4 <- hhh4(noroBE_power, ctrl4)
AIC(fit4)


## ----neweights,cache=TRUE, fig.height=4--------------------------------------------------------------------------
# visualize the nighbourhood weights:
plot(fit4, type = "neweights", main = "Weights by path distance")


## ----fit5, cache=TRUE--------------------------------------------------------------------------------------------
##################################################################
# Model 5:
ctrl5 <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              # now adding seasonality to the ne component:
              ne = list(f =  addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1),
                        weights = W_powerlaw(maxlag=5, normalize = TRUE,
                                             log=TRUE)), # this is new
              family = "NegBin1",
              subset = subset_fit)
fit5 <- hhh4(noroBE_power, ctrl5)
AIC(fit5)


## ----pr5, cache=TRUE---------------------------------------------------------------------------------------------
# compute Pearson residuals:
pr5 <- pearson_residuals(fit4)
par(mfrow = c(3, 4))
for(unit in colnames(pr5)){
  acf(pr5[, unit], lag.max = 5, ylim = c(-0.3, 0.3), main = unit)
}


## ----lag_types, fig.height=4, cache=TRUE-------------------------------------------------------------------------
# check out examples of the different lag types:
# geometric:
(g <- geometric_lag(par_lag = 1.5, min_lag = 1, max_lag = 5))
# first weight corresponds to exp(1)/(1 + exp(1))
# 5 is also the default number of lags

# Poisson:
(p <- poisson_lag(par_lag = 0.8, min_lag = 1, max_lag = 5))
# weights correspond to dpois(0:5, exp(1))/sum(dpois(0:4, exp(1)))

par(mfrow = 1:2)
plot(g, xlab = "lag", ylab = "weight", ylim = 0:1, type = "h", 
     main = "Geometric weights")
plot(p, xlab = "lag", ylab = "weight", ylim = 0:1, type = "h", 
     main = "Poisson weights")
# moreover a two-point distribution and a triangular distribution. Users can
# also provide their own weighting functions.


## ----fit6, cache=TRUE--------------------------------------------------------------------------------------------
##################################################################
# Model 6
ctrl6 <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              ne = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                              S = 1),
                        weights = W_powerlaw(maxlag=5, normalize = TRUE,
                                             log=TRUE)),
              family = "NegBin1",
              subset = subset_fit,
              par_lag = 1.5, # this is new
              funct_lag = geometric_lag) # this is new; geometric_lag is default
fit6 <- hhh4_lag(noroBE_power, ctrl6)
AIC(fit6)


## ----fit7, cache=TRUE--------------------------------------------------------------------------------------------
##################################################################
# Model 7
ctrl7 <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              # note: line ar = ... is removed
              ne = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                              S = 1),
                        weights = W_powerlaw(maxlag=5, normalize = TRUE,
                                             log=TRUE)), # this is new
              family = "NegBin1",
              subset = subset_fit,
              # (no specification of par_lag in the control)
              funct_lag = geometric_lag)
# now use profile_par_lag (applies a profile likelihood procedure to estimate
# the lag decay parameter)
fit7 <- profile_par_lag(noroBE_power, ctrl6)
AIC(fit7)


## ----plot_weights, fig.height=4, cache=TRUE----------------------------------------------------------------------
# plot the weights assigned to the different lags:
par(mfrow = 1:2)
plot(fit7$distr_lag, type = "h", xlab  = "lag",
     ylab = "weight", ylim = 0:1)


## ----pr6, cache=TRUE---------------------------------------------------------------------------------------------
# check Pearson residuals
pr7 <- pearson_residuals(fit6)
par(mfrow = c(3, 4))
for(unit in colnames(pr7)){
  acf(pr7[, unit], lag.max = 5, ylim = c(-0.3, 0.3), main = unit)
}


## ----owa, cache=TRUE, fig.height=4-------------------------------------------------------------------------------
##################################################################
# one-step-ahead forecasting: generate forecasts sequentially
# compare models 2, 4 and 7
log2 <- capture.output(owa2 <- oneStepAhead(fit2, tp = c(312, 363)))
log4 <- capture.output(owa4 <- oneStepAhead(fit4, tp = c(312, 363)))
rm(log2, log4)
# the weird capture.output fourmulation is needed to suppress 
# numerous cat() messages.
# you could also just use
# owa2 <- oneStepAhead(fit2, tp = c(312, 363))
owa7 <- oneStepAhead_hhh4lag(fit7, tp = c(312, 363))

# the return objects contain predictions, observations and a few other things:
head(owa7$pred)
head(owa7$observed)

# plot one-step-ahead point forecasts:
plot(noroBE@observed[313:364, 1], pch = 20, xlab = "week", ylab = "value")
lines(owa2$pred[, 1], type = "l", col = 2)
lines(owa4$pred[, 1], type = "l", col = 4)
lines(owa7$pred[, 1], type = "l", col = 6)

# compute and summarize scores:
colMeans(scores(owa2, which = c("logs", "rps")))
colMeans(scores(owa4, which = c("logs", "rps")))
colMeans(scores(owa7, which = c("logs", "rps")))


## ----moments, cache=TRUE, fig.height=4---------------------------------------------------------------------------
##################################################################
# longer-term predictive moments can be computed using predictive_moments:
# predictive moments 10 weeks ahead:
pred_mom7 <- predictive_moments(fit7, t_condition = max(subset_fit), lgt = 10)
# print some predictive means:
head(pred_mom7$mu_matrix[, 1:6])
# plot:
plot(fit7)
fanplot_prediction(pred_mom7, add = TRUE,
                   probs = c(0.05, 0.15, 0.25, 0.75, 0.85, 0.95),
                   pt.col = "black")
# note: the plot is based on a negative binomial approximation of the 
# predictive distributions.

# stationary/marginal moments are implemented, too (but don't always exist):
stat7 <- stationary_moments(fit6)
fanplot_stationary(stat7, unit = 4)


## ----rotaBE------------------------------------------------------------------------------------------------------
data("rotaBE")
# if you don't have the hhh4addon packae installed you can use:
# load(
# url("https://github.com/cmmid/hhh4-workshop/raw/main/example_noro_rota/rotaBE.Rda")
# )


## ----rota_example------------------------------------------------------------------------------------------------
ctrl <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                               S = 1)),
              ar = list(f = ~ 0 + fe(1, unitSpecific = TRUE)),
              family = "NegBin1",
              subset = 6:312)


## ----rota_example2-----------------------------------------------------------------------------------------------
your_fit <- hhh4(rotaBE, ctrl)


## ----eval--------------------------------------------------------------------------------------------------------
# owa <- oneStepAhead(your_fit, tp = c(312, 363))
# or: owa <- oneStepAhead_hhh4(your_fit, tp = c(312, 363))
# if you have used profile_par_lag 
# colMeans(scores(owa), which = c("logs", "rps"))
#     logs       rps
# 1.931189  1.639351


## ----load_temp, cache=TRUE---------------------------------------------------------------------------------------
# get temperature data:
data_temperature <-
  read.csv(paste0("https://raw.githubusercontent.com/cmmid/hhh4-workshop/", 
                  "main/example_noro_rota/temperature_berlin.csv"))
temperature <- data_temperature$temperature7d
# your formula could look as follows:
ctrl <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                              S = 1)),
             ar = list(f = ~ 0 + temperature + fe(1, unitSpecific = TRUE)),
             family = "NegBin1",
             subset = 6:312)
# though that is not necessarily a very smart way of using the covariate

