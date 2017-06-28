
#' @title Construct a KD-Tree using only basic vanilla R functionality
#' @description This algorithm builds a KD-Tree. NB currently duplicate values can cause undefined behaviour!
#' @param X Data frame of values on which to perform splits
#' @param y Vector or data frame with data that should be stored in the leafs
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
#' @param X Data frame of values on which to perform splits
#' @param y Vector or data frame with data that should be stored in the leafs
#' @param FUN Optional function to use for determining position of split. Defaults to median as this gives a balanced tree
#' @param ... Optional arguments to pass on to FUN
#' @author Jurian Baas
#' @export
#' @importFrom data.tree Node
#' @importFrom stats median
buildKdDataTree <- function(X, y, FUN = median, ...) {

  if(!is.data.frame(X)) stop("First argument has to be a data frame")

  # Nr of dimensions
  d <- ncol(X)

  build <- function(X, y, depth, parent, side, FUN) {

    if(nrow(X) == 1) {
      y <- y[1,]
      leaf <- parent$AddChild(
        paste(y, collapse = ","),
        value = y,
        side = side,
        depth = depth
      )

    } else {

      i <- depth %% d + 1

      split <- FUN(X[[i]], ...)
      idx.1 <- X[[i]] <= split
      idx.2 <- !idx.1

      if(is.null(parent)) {
        root <- data.tree::Node$new(
          as.character(split),
          split = split,
          dim = i,
          side = 'root',
          depth = depth
        )

        if(sum(idx.1) > 0 & sum(idx.2) > 0) {
          build(X[idx.1], y[idx.1], depth + 1, root, 'left', FUN)
          build(X[idx.2], y[idx.2], depth + 1, root, 'right', FUN)
        }

        return(root)

      } else {
        child <- parent$AddChild(
          as.character(split),
          split = split,
          dim = i,
          side = side,
          depth = depth
        )

        if(sum(idx.1) > 0 & sum(idx.2) > 0) {
          build(X[idx.1], y[idx.1], depth + 1, child, 'left', FUN)
          build(X[idx.2], y[idx.2], depth + 1, child, 'right', FUN)
        }


      }
    }
  }

  kd <- list()
  kd$tree <- build(X, y, depth = 0, parent = NULL, side = NULL, FUN = FUN)
  kd$X = X
  kd$dim <- d

  class(kd) <- "kd.data.tree"

  return(kd)

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
  if(kdtree$dim != 2) stop("Currently only two dimensional trees can be plotted")

  if(plotting.type == "scatter") {

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

        test <- c(
          "left left right",
          "left right right",
          "right right left",
          "right left left"
        )

        if(node$depth >= 3) {

          anc <- paste(
            node$side,
            node$parent$side,
            node$parent$parent$side
          )

          print(
            anc
          )

          print(anc %in% test)
        }

      }



      if(!node$isLeaf & node$depth < 4) {

        # If it's a vertical line
        if(node$dim == 1) {

          if(node$isRoot) {
            a <- box.y[1]
            b <- box.y[2]
          } else if(node$side == 'right') {
            a <- ifelse(is.null(node$parent), box.y[1], node$parent$split)
            b <- ifelse(is.null(node$parent$parent$parent), box.y[2], node$parent$parent$parent$split)
          } else if(node$side == 'left') {
            a <- ifelse(is.null(node$parent), box.y[2], node$parent$split)
            b <- ifelse(is.null(node$parent$parent$parent), box.y[1], node$parent$parent$parent$split)
          }

          y <- c(a,b)

          graphics::lines(
            x = c(node$split, node$split),
            y = y
          )

          graphics::text(y = mean(y), x = node$split, label = paste(node$depth, node$side))

        } else {

          if(node$side == 'right') {
            a <- ifelse(is.null(node$parent), box.x[1], node$parent$split)
            b <- ifelse(is.null(node$parent$parent$parent), box.x[2], node$parent$parent$parent$split)
          } else if(node$side == 'left') {

            a <- ifelse(is.null(node$parent), box.x[2], node$parent$split)
            b <- ifelse(is.null(node$parent$parent$parent), box.x[1], node$parent$parent$parent$split)
          }

          x <- c(a,b)


          graphics::lines(
            y = c(node$split, node$split),
            x = x
          )

          graphics::text(x = mean(x), y = node$split, label = paste(node$depth, node$side))
        }

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
