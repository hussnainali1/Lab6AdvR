
#' greedy_knapsack
#'
#' @param x A DataFrame
#' @param W an Integer
#'
#' @return A list
#' @export greedy_knapsack
#'
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  tempDataFrame <- x
  objW <- x$w
  objv <- x$v
  divionRes <- objv / objW
  tempDataFrame$divRes <- divionRes

  tempDataFrame <-  tempDataFrame[order(tempDataFrame$divRes, decreasing = TRUE),]
  summ <- 0
  itemVector <- c()
  allowedWeight <- W
  tempSumm <- 0
  for (index in 1:nrow(tempDataFrame)) {

    if(allowedWeight > tempDataFrame[index,1]){
      rn <- as.numeric(rownames(tempDataFrame[index,]))
      summ <- summ + tempDataFrame[index,2]
      allowedWeight <- allowedWeight - tempDataFrame[index,1]
      # cat(paste("item=", rn , " allocated weight left ", allowedWeight, " summm = ",summ ,"\n"))
      itemVector <- append(itemVector,rn)
      tempSumm <- tempDataFrame[index,2]
    }

  }
    summ <- summ - tempSumm
    itemVector <- itemVector[1:length(itemVector)-1]
  return(list("value"= summ, "elements"= itemVector))
}

# RNGversion(min(as.character(getRversion()),"3.5.3"))
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )


