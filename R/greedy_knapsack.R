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
  stopifnot(W>0)

  tempDataFrame <- x
  objW <- x$w
  objv <- x$v
  finalValue <- 0
  knapsack_value <- 0
  Summ <- 0
  divionRes <- objv / objW
  tempDataFrame$divRes <- divionRes

  tempDataFrame <-  tempDataFrame[order(tempDataFrame$divRes, decreasing = TRUE),]
  knapsack_items <- c()
  for (i in 1:nrow(x)) {
    Summ <- Summ + tempDataFrame[i,1]

    if(Summ <= W){
      finalValue <- finalValue + tempDataFrame[i,2]
    }

    if(finalValue > knapsack_value){
      knapsack_value <- finalValue
      indexNames <- rownames(tempDataFrame)[i]
      indexNames <- as.integer(indexNames)
      knapsack_items <- c(knapsack_items, indexNames)
    }
  }
  return(list("value"= knapsack_value, "elements"= knapsack_items))
}

# system.time(greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))

# RNGversion(min(as.character(getRversion()),"3.5.3"))
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 1000000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )


