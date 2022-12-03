#' mycltfun()
#'
#' @param n n parameter
#' @param iter iteration

#' @importFrom stats runif
#' @return a histogram
#' @export
#'
#' @examples mycltfun(n=50,iter=10000)
mycltfun=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,mean) #C
  hist(sm)
  return(sm)
}

