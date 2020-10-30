#' @title A brute-force 0/1 solution for the knapsack problem
#' @name brute_force_knapsack
#' @description The function calculates the combination of items to pack in a
#' knapsack to maximize the value in the knapsack given a weight restraint.
#' @param x 'x' is a dataframe with observations of value of item and weight of item.
#' @param W 'W' is the maximum weight.
#' @return Return the items that maximaze the value of the knapsack given the weight restraint, as well as the value of the knapsack.
#' @example brute_force_knapsack(x = knapsack_objects [1: 8,], W = 3500)
#' @export

set.seed (42)
n <- 2000
knapsack_objects <- data.frame(w = sample (1: 4000, size = n, replace = TRUE),v = runif(n = n, 0, 10000))

brute_force_knapsack <- function (x, W){
  if(is.data.frame(x)==F) stop( "x is not a dataframe")
  if(any(variable.names(x)!=c("w","v"))) stop("incorrect variables")
  if(any(x$v <0) | any(x$w)<0) stop("non-positive-numbers")

  #tar fram alla kombinationer av element
  number_of_items <- nrow(x)
  all_combinations_decimal <- 0:(2^number_of_items-1)
  all_combinations_matrix <- sapply(all_combinations_decimal, function(x){
    result <- intToBits(x)
    result <- as.numeric(result)
    return(result[1:number_of_items])
  })

  slutgiltigt_value <- 0 #skapar det som skall bli slutgiltigt resultat

  index_vek <- c()
  for (i in 2:ncol(all_combinations_matrix)){ #börjar på 2 eftersom i kolumn 1 är alla element 0
    for(j in 1:nrow(all_combinations_matrix)){
      if(all_combinations_matrix[j, i]==1){
        index_vek <- c(index_vek, j) #alla element i en kolumn som har en etta i kombinationsmatrisen sparas som sitt radnummer
      }
    }
    weight_per_col_iteration <- sum(x$w[index_vek])
    value_per_col_iteration <- sum(x$v[index_vek])
    if((weight_per_col_iteration <=W & value_per_col_iteration > slutgiltigt_value)==TRUE){
      slutgiltigt_value <- value_per_col_iteration
      slutgiltig_vikt <- weight_per_col_iteration
      slutgiltiga_element <- index_vek
    }
    index_vek <- c() #återställer indexvektorn för nästa kolumn-iteration
  }
  lista <- list("value"= round(slutgiltigt_value, digits = 0), "weight" =slutgiltig_vikt, "elements"=slutgiltiga_element)
  return(lista)
}
