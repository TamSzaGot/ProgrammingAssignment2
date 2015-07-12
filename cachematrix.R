## The following pair of functions makes it possible to cache the
## value of the time-consuming operation of matrix inversion and
## can be used as follows:
##
## > A = matrix( 
## +     c(1, 2, 3, 4), 
## +     nrow=2, 
## +     ncol=2)
##
## > cA = makeCacheMatrix(A)
## > cA$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > IA = cacheSolve(cA)
##
## > IA
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## To see the effect of caching, create a large sample matrix:
## > B = matrix(sample(1000,1000000,T),1000)
## > cB = makeCacheMatrix(B)
## > IB = cacheSolve(cB)
## (can take some time to compute depending on your hardware ...)
## > IB = cacheSolve(cB)
## getting cached data
## (the seconbd call returns the cached inverted matrix at once)
##



## Creates a cached matrix representation whereby the inverese
## of the matrix can be cached for fast retrieval

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns inverse of the matrix
## supplied as cached matrix representation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
