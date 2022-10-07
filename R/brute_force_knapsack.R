#' brute_force_knapsack
#'
#' @param x A DataFrame
#' @param W an Integer
#'
#' @return A list
#' @export brute_force_knapsack
#'
#' @examples
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
brute_force_knapsack <-function(x, W){
    stopifnot(is.data.frame(x))
    stopifnot(is.numeric(W))
    n <- length(x$v)
    Weights <- x$w
    Values <- x$v
    finalValue <- 0
    tempVec <-rep(0,times=length(x$w))
    for (i in 1:2^n) {
      j <-  n
      tempWeight <-  0
      tempValue <-  0
      while (tempVec[j] != 0 && j > 0){
        tempVec[j] <-  0
        j <-  j - 1
      }
      tempVec[j] <-  1
      for (k in 1:n) {
        if (tempVec[k] == 1){
          tempWeight <-  tempWeight + Weights[k]
          tempValue <-  tempValue + Values[k]
        }
      }

      if ((tempValue > finalValue) && (tempWeight <=  W)){
        finalValue <-  tempValue
        bestWeight <-  tempWeight
        nodes <-  tempVec
      }
    }
    nodes <- which(nodes==1,  arr.ind = TRUE)
    return (list("value"=finalValue, "output"=nodes))
  }

