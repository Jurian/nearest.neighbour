

buildKdTree <- function(P, data, FUN = median, ...) {

  # Nr of dimensions
  d <- ncol(P)

  build <- function(P, data, depth, tree, FUN) {

    if(nrow(P) == 1) return(as.vector(data))

    i <- depth %% d + 1

    split <- FUN(P[[i]], ...)
    idx.1 <- P[[i]] <= split
    idx.2 <- !idx.1

    tree$left <- build(P[idx.1], data[idx.1], depth + 1, tree, FUN)
    tree$right <- build(P[idx.2], data[idx.2], depth + 1, tree, FUN)

    return(tree)

  }

  tree <- build(P, data, depth = 0, tree = list(), FUN = FUN)

  return(tree)

}

buildBinaryTree <- function(P) {

  build <- function(P, tree) {

    if(length(P) == 1) return(P)

    idx <- 1:length(P) / 2

    tree$left <- build(P[idx], tree)
    tree$right <- build(P[-idx], tree)

    return(tree)
  }

  tree <- build(P, tree = list())

  return(tree)
}
