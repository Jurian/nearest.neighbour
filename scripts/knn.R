library(kknn)
library(ggplot2)
require(reshape2)
library(KernelKnn)
library(devtools)
install_github("mlampros/RandomSearchR")
source("R/dataPreperation.R")

ggplot.kknn.mse <- function(knn) {

  mse <- sapply(knn, function(x){x$MEAN.SQU})

  idx <- which(mse == min(mse), arr.ind = T)

  mse <- data.table(mse)
  mse$k <- 1:nrow(mse)

  ggplot(melt(mse ,  id.vars = 'k', variable.name = 'series'), aes(k,value)) +
    geom_line(aes(colour = series), size = 1) +
    geom_point(aes(colour = series)) +
    geom_vline(xintercept = idx[1], color = "red", linetype="dotted")
  #annotate("text", label = colnames(mse)[idx[2]] , x = idx[1]+0.1, y = mean(mse[[idx[2]]]), color = "red")

}

dat <- combine.weather.datasets(
  station.file = "data/stations.csv",
  weather.files = c("data/weather.csv"),
  columns = c("datetime", "longitude", "latitude",  "TG"))

# Fix rain < 0.1mm
# NB No rain atm
#dat$RH[dat$RH < 0] <- 0

# Remove NA's
dat <- dat[complete.cases(dat)]
# Full set takes too long for me, disable this if you have time/cpu power to spare
dat <- dat[sample(1:nrow(dat), 100000)]

# A randomized search simply samples parameter settings a fixed number of times from a specified subset of the hyperparameter space of a learning algorithm.
# This method has been found to be more effective in high-dimensional spaces than an exhaustive search (grid-search).

grid_kknn = list(
  k = 1:8,
  distance = c(1, 2),
  kernel = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))

# Random search has a probability of 95% of finding a combination of parameters within the 5% optima with only 60 iterations.
# See accepted answer at: https://stats.stackexchange.com/questions/160479/practical-hyperparameter-optimization-random-vs-grid-search
# i.e. 1 − (1−0.05)^n > 0.95 = n >= 60
# 60 iterations is 37.5% of an exhaustive search in this case (8 * 2 * 10)
# 90 iterations (56.25% of an exhaustive search) would get you 99% probability of being in 5% optimum
res_kknn = RandomSearchR::random_search_resample(dat$TG, tune_iters = 60,
                                 resampling_method = list(method = 'cross_validation', folds = 5),
                                 ALGORITHM = list(package = require(kknn), algorithm = kknn),
                                 grid_params = grid_kknn,
                                 DATA = list(formula = TG ~ datetime + longitude + latitude, train = dat),
                                 Args = NULL,
                                 regression = T, re_run_params = F)

perf = RandomSearchR::performance_measures(
  list_objects = list(kknn = res_kknn),
  eval_metric = RandomSearchR::mse,
  sort = list(variable = 'Median', decreasing = T))


best_kknn = RandomSearchR::subset_mods(
  perf_meas_OBJ = perf,
  bst_mods = 5,
  train_params = F)

# Random search has a probability of 95% of finding a combination of parameters within the 5% optima with only 60 iterations.
# This time re_run_params is set to TRUE
res_kknn = RandomSearchR::random_search_resample(dat$TG, tune_iters = 60,
                                                 resampling_method = list(method = 'cross_validation', folds = 5),
                                                 ALGORITHM = list(package = require(kknn), algorithm = kknn),
                                                 grid_params = best_kknn,
                                                 DATA = list(formula = TG ~ datetime + longitude + latitude, train = dat),
                                                 Args = NULL,
                                                 regression = T, re_run_params = T)

