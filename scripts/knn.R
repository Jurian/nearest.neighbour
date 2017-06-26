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
  columns = c("datetime", "longitude", "latitude", "RH", "TG"))

# Fix rain < 0.1mm
dat$RH[dat$RH < 0] <- 0

# A randomized search simply samples parameter settings a fixed number of times from a specified subset of the hyperparameter space of a learning algorithm.
# This method has been found to be more effective in high-dimensional spaces than an exhaustive search (grid-search).

grid_kknn = list(
  k = 3:20,
  distance = c(1, 2),
  kernel = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))

res_kkn = RandomSearchR::random_search_resample(dat$TG, tune_iters = 30,
                                 resampling_method = list(method = 'bootstrap', repeats = 25, sample_rate = 0.65, folds = NULL),
                                 ALGORITHM = list(package = require(kknn), algorithm = kknn),
                                 grid_params = grid_kknn,
                                 DATA = list(formula = TG ~ datetime + longitude + latitude, train = dat),
                                 Args = NULL,
                                 regression = T, re_run_params = F)





