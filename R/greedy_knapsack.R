
knapsack_objects_fun <- function(x, W){
  # stopifnot(is.data.frame(x))
  # stopifnot(is.numeric(W))
  tempDataFrame <- x
  objW <- x$w
  objv <- x$v
  divionRes <- objv / objW
  tempDataFrame$divRes <- divionRes

  tempDataFrame <-  tempDataFrame[order(tempDataFrame$divRes, decreasing = TRUE),]
  summ <- 0
  itemVector <- c()
  allowedWeight <- W

  for (index in 1:nrow(tempDataFrame)) {

    if(allowedWeight > tempDataFrame[index,1]){
      rn <- as.numeric(rownames(tempDataFrame[index,]))
      summ <- summ + tempDataFrame[index,2]
      allowedWeight <- allowedWeight - tempDataFrame[index,1]
      itemVector <- append(itemVector,rn)
    }

  }
  return(list("value"= summ, "elements"= itemVector))
}

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_objects_fun(x = knapsack_objects[1:1200,], W = 2000)
