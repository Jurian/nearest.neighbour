library(kknn)
library(ggplot2)
require(reshape2)

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

source("R/dataPreperation.R")

dat <- combine.weather.datasets(
  station.file = "data/stations.csv",
  weather.files = c("data/weather.csv"),
  columns = c("datetime", "longitude", "latitude", "RH", "TG"))

knn.tg <- list()
knn.tg$rectangular  <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "rectangular")
knn.tg$triangular   <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "triangular")
knn.tg$epanechnikov <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "epanechnikov")
knn.tg$biweight     <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "biweight")
knn.tg$triweight    <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "triweight")
knn.tg$cos          <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "cos")
knn.tg$inv          <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "inv")
knn.tg$gaussian     <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "gaussian")
knn.tg$optimal      <- train.kknn(TG ~ datetime + longitude + latitude, data = dat, kernel = "optimal")

ggplot.kknn.mse(knn.tg)

knn.rh <- list()
knn.rh$rectangular  <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "rectangular")
knn.rh$triangular   <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "triangular")
knn.rh$epanechnikov <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "epanechnikov")
knn.rh$biweight     <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "biweight")
knn.rh$triweight    <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "triweight")
knn.rh$cos          <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "cos")
knn.rh$inv          <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "inv")
knn.rh$gaussian     <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "gaussian")
knn.rh$optimal      <- train.kknn(RH ~ datetime + longitude + latitude, data = dat, kernel = "optimal")

ggplot.kknn.mse(knn.rh)




