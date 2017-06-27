library(KernelKnn)
library(devtools)
install_github("mlampros/RandomSearchR")


grid = list(nhid = 5:50, actfun = c('sig', 'sin', 'purelin', 'radbas', 'poslin'))



