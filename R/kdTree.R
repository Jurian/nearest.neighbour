
#' @title Construct a KD-Tree using only basic vanilla R functionality
#' @description This algorithm builds a KD-Tree. NB currently duplicate values can cause undefined behaviour!
#' @param X Data frame of values on which to perform splits
#' @param y Vector with data that should be stored in the leafs
#' @param FUN Optional function to use for determining position of split. Defaults to median as this gives a balanced tree
#' @param ... Optional arguments to pass on to FUN
#' @author Jurian Baas
#' @export
#' @importFrom data.tree Node
buildKdTree <- function(X, y, FUN = median, ...) {

  # Nr of dimensions
  d <- ncol(X)

  build <- function(X, y, depth, tree, FUN) {

    if(nrow(X) == 1) return(as.vector(y))

    i <- depth %% d + 1

    split <- FUN(X[[i]], ...)
    idx.1 <- X[[i]] <= split
    idx.2 <- !idx.1

    tree  <- c(tree, list(
      value = split,
      dim = i,
      left = build(X[idx.1], y[idx.1], depth + 1, tree, FUN),
      right = build(X[idx.2], y[idx.2], depth + 1, tree, FUN)
    ))

    return(tree)

  }

  kd <- list()
  kd$tree <- build(X, y, depth = 0, tree = list(), FUN = FUN)
  kd$points = X
  kd$dim <- d

  class(kd) <- "kd.tree"

  return(kd)

}

#' @title Construct a KD-Tree using the data.tree package
#' @description This algorithm works similarly as buildKdTree but allows for more fancy operation such as child-parent traversal
#' @param X Data table of values on which to perform splits
#' @param y Vector with data that should be stored in the leafs
#' @param scale Should data be scaled beforehand, defaults to true
#' @param FUN Optional function to use for determining position of split. Defaults to median as this gives a balanced tree
#' @param ... Optional arguments to pass on to FUN
#' @author Jurian Baas
#' @export
#' @importFrom data.tree Node
#' @importFrom stats median
#' @importFrom data.table data.table
buildKdDataTree <- function(X, y, scale = TRUE, FUN = median, ...) {

  if(scale) X <- data.table::data.table(scale(X))

  # Nr of dimensions
  d <- ncol(X)

  build <- function(X, y, depth, parent, FUN) {

    if(nrow(X) == 1) {

      leaf <- parent$AddChild(
        paste(y, collapse = ","),
        value = y,
        depth = depth
      )

    } else {

      # Iterate over dimensions
      i <- depth %% d + 1

      # compute split
      split <- FUN(X[[i]], ...)
      idx.1 <- X[[i]] <= split
      idx.2 <- !idx.1

      node.name <- paste(colnames(X)[i], "=", split)

      if(is.null(parent)) {
        node <- data.tree::Node$new (
          node.name,
          split = split,
          dim = i
        )
      } else {
        node <- parent$AddChild (
          node.name,
          split = split,
          dim = i
        )
      }

      # Build subtree recursively
      build(X[idx.1], y[idx.1], depth + 1, node, FUN)
      build(X[idx.2], y[idx.2], depth + 1, node, FUN)

      if(is.null(parent)) return(node)

    }
  }

  kd <- list()
  kd$tree <- build(X, y, depth = 0, parent = NULL, FUN = FUN)
  kd$X = X
  kd$dim <- d

  class(kd) <- "kd.data.tree"

  return(kd)

}

#' @title Composite of an n-dimensional point
#' @description Calculate the associated point in the composite number space
#' @param p Point to calculate composite of
#' @return The composite of the given point
#' @author Jurian Baas
composite <- function(p) {
  return(c(as.numeric(paste0(p, collapse = '')),as.numeric(paste0(rev(p), collapse = ''))))
}

#' @title Plot a scatter plot or a tree representation of a two dimensional KD-Tree
#' @description If plotting.type = "scatter", plots a scatter plot of a two dimensional KD-Tree by drawing the coordinate data as points and adding lines where the data is split. Otherwise, if plotting.type = "tree", draws the two dimensional KD-Tree as a top down tree. Leafs are made square as per convention.
#' @param x An object of type kd.data.tree
#' @param ... Optional arguments to pass on
#' @param plotting.type One of c("scatter", "tree")
#' @author Jurian Baas
#' @importFrom graphics plot text lines par
#' @export
plot.kd.data.tree <- function(x, ..., plotting.type = "scatter") {

  kdtree <- x

  if(class(kdtree) != "kd.data.tree") stop("Parameter is not of class kd.data.tree")

  if(plotting.type == "scatter") {

    if(kdtree$dim != 2) stop("Only two dimensional trees can be plotted this way")

    graphics::plot(kdtree$X,
                   col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)
    graphics::text(kdtree$X,
                   labels = paste(kdtree$X[[1]], kdtree$X[[2]], sep = ","),
                   cex = 0.7, pos = 4)

    box <- graphics::par("usr")
    box.x <- box[1:2]
    box.y <- box[3:4]

    kdtree$tree$Do(function(node) {

      if(!node$isLeaf) {

      }
    })

  } else if(plotting.type == "tree") {

    data.tree::Do(kdtree$tree$leaves, function(node) data.tree::SetNodeStyle(node, shape = "square"))
    graphics::plot(kdtree$tree, ...)

  }

}


#' @title Plot an object of type kd.tree
#' @description Not working ATM, please use plot.kd.data.tree instead
#' @param x An object of type kd.tree to plot
#' @param ... Optional arguments to pass on
#' @author Jurian Baas
#' @importFrom graphics plot text lines
plot.kd.tree <- function(x, ...) {

  # The x, ... arguments are there to conform to the CRAN standard, see also:
  # https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Generic-functions-and-methods
  kdtree <- x

  if(class(kdtree) != "kd.tree") stop("Parameter is not of class kd.tree")
  if(kdtree$dim != 2) stop("Currently only two dimensional trees can be plotted")

  graphics::plot(kdtree$X,
                 col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)
  graphics::text(kdtree$X,
                 labels = paste(kdtree$X[[1]], kdtree$X[[2]], sep = ","),
                 cex = 0.7, pos = 4)

  traverseKdTree(kdtree, function(node) {

    vertical <- node$dim %% 2

    if(vertical) {
      graphics::lines(
        x = c(node$value, node$value),
        y = range(kdtree$X[,2]))
    } else {
      graphics::lines(
        x = range(kdtree$X[,1]),
        y = c(node$value, node$value))
    }

  })
}

#' @title Traverse an object of type kd.tree
#' @description Traverses the KD-Tree recursively, applying the function FUN to each node
#' @param kdtree An object of type kd.tree
#' @param FUN This function will be given a node as the first parameter
#' @param ... Optional arguments to pass on to FUN
#' @author Jurian Baas
#' @export
traverseKdTree <- function(kdtree, FUN, ...) {

  if(class(kdtree) != "kd.tree") stop("Parameter is not of class kd.tree")

  traverse <- function(subtree) {
    FUN(subtree, ...)

    if(is.list(subtree$left)) traverse(subtree$left)
    if(is.list(subtree$right)) traverse(subtree$right)
  }

  traverse(kdtree$tree)

}
