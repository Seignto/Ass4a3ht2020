#' @title A greedy heuristics aproximation for the knapsack problem
#' @name greedy_knapsack
#' @description The function calculates the combination of items to pack in a
#' knapsack to maximize the value in the knapsack.
#' @param x x' is a dataframe with observations of value of item and weight of item.
#' @param W 'W' is the maximum wheight.
#' @return The combination of items that maximizes the value packed given a
#' wheight restriction
#' @example greedy_knapsack (x = knapsack_objects [1: 800,], W = 3500)
#' @export

set.seed (42)
n <- 2000
knapsack_objects <- data.frame(w = sample (1: 4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))

greedy_knapsack <- function(x, W){
  if(is.data.frame(x)==F) stop( "x is not a dataframe")
  if(any(variable.names(x)!=c("w","v"))) stop("incorrect variables")
  if(any(x$v <0) | any(x$w)<0) stop("non-positive-numbers")

  #sorterar df
  value_weight <- x$v/x$w
  x <- cbind(x, value_weight)
  x<- x[order(x$value_weight, decreasing = TRUE) ,]


  #startvärden för while-loop
  i <- 1
  sorted_elements <- c()
  total_value <- 0

  while (sum(x$w[1:i]) < W) {
    total_value <- sum(x$v[1:i])
    sorted_elements <- c(sorted_elements, i)
    i <- i+1

  }
  original_elements <- as.numeric(rownames(x[sorted_elements, ]))
  lista <- list(value=total_value, elements=original_elements)
  return(lista)

}
