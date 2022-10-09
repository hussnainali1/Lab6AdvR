#' dynamic_knapsack
#'
#' @param x A DataFrame
#' @param W an Integer
#'
#' @return A list
#' @export dynamic_knapsack
#'
#' @examples
#' dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)

dynamic_knapsack <- function(x,W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))

  n <- length(x$w)
  matrixx <- matrix(nrow = n, ncol = W)
  tempknapItems <-c()
  allowedWeight <- W

  for(Columss in 1:n){
    for(Rowss in 1:W){
      if(Columss == 1 | Rowss == 1){
        matrixx[Columss, Rowss] <- 0
      }
      else if(x[["w"]][Columss] > Rowss){
        matrixx[Columss, Rowss] <- matrixx[Columss - 1, Rowss]
      }
      else {
        maxVal <- max(matrixx[Columss - 1, Rowss], matrixx[Columss - 1, Rowss - x[["w"]][Columss]] + x[["v"]][Columss])
        matrixx[Columss, Rowss] <- maxVal
      }
    }
  }
  tempValue <- matrixx[n, W]

  for(i in n:1){
    if(tempValue == matrixx[i - 1, allowedWeight]){
      next
    }
    else{
      tempknapItems <- c(tempknapItems, x[["w"]][i])
      tempValue <- tempValue - x[["v"]][i]
      allowedWeight <- allowedWeight - x[["w"]][i]
    }
    if(tempValue <= 0){
      break
    }
  }
  finalElement <- which(x[["w"]] %in% tempknapItems)
  finalresult <- list("value" = matrixx[n, W], "elements" = finalElement)
  return(finalresult)
}

