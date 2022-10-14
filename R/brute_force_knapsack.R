#' brute_force_knapsack
#'
#' @param x A DataFrame
#' @param W an Integer
#' @param parallel A Boolean
#'
#' @return A list
#' @export brute_force_knapsack
#'
#' @examples
#'brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
brute_force_knapsack<-function(x,W, parallel=FALSE)
{
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  stopifnot(W>0)

  endRes = list()
  n<-length(x$v)
  value<-0
  elements<-c()
  tempMaximum<-0
  if (!parallel)
  {
    lapply(1:n, function(i)
    {
      macComb <-utils::combn(n,i)
      pp<-1
      while(pp <= ncol(macComb))
      {
        if(sum(x$w[macComb[,pp]]) <= W)
        {
          value<-sum(x$v[macComb[,pp]])
          if(value > tempMaximum)
          {
            tempMaximum <<- value
            elements <<- macComb[,pp]
          }
        }
        pp<-pp+1
      }
    })
    endRes<-list(value=round(tempMaximum),elements=elements)
  }

  else
  {
    nodes <- parallel::makeCluster(parallel::detectCores()/2)
    parallel::clusterExport(nodes, varlist=c("x","W","n","value","tempMaximum","elements"), envir=environment())
    parallel::clusterEvalQ(nodes, library(utils))
    Value <- parallel::parLapply(nodes, 1:n, function(i, x, W) {
      macComb <- utils::combn(n,i)
      pp <- 1
      while(pp<=ncol(macComb))
      {
        if(sum(x$w[macComb[,pp]]) <= W)
        {
          value<-sum(x$v[macComb[,pp]])
          if(tempMaximum<value)
          {
            elements<-macComb[,pp]
            tempMaximum<-value
          }
        }
        pp <- pp+1
      }

      return(list(value=round(tempMaximum), elements = elements))

    }, x, W )

    qq=1
    while(Value[[qq]]["value"]!=0)
    {
      elements<-Value[[qq]]["elements"]
      value<-Value[[qq]]["value"]
      qq<-qq+1
    }
    endRes= c(value,elements)
    parallel::stopCluster(nodes)
  }

  return (endRes)
}







