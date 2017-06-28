#' @title Build a binary tree
#' @description Toy example, not used ATM
#' @param X Data frame to use
#' @author Jurian Baas
buildBinaryTree <- function(X) {

  build <- function(X, tree) {

    if(length(X) == 1) return(X)

    idx <- 1:length(X) / 2

    tree$left <- build(X[idx], tree)
    tree$right <- build(X[-idx], tree)

    return(tree)
  }

  tree <- build(X, tree = list())

  return(tree)
}
