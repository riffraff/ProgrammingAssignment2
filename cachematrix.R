## A "matrix" that carries an embedded cached value of it's inverse

# the matrix is implemented as a "poor man's object" i.e. a record of
# closures with a shared mutable environment. 
# the two functions set() and get() allow accessing and mutating the 
# embedded matrix object, while set.inverse and get.inverse allow 
# manipulation of the inverse object in the shared environment.
#
#


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set.inverse <- function(result) inverse <<- result
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


# Cache solve can be used to get the inverse of a matrix 
# in a smart way by looking up the cached value and memoizing 
# it after a computation for future use. 

cacheSolve <- function(matrix, ...) {
  res <- matrix$get.inverse()
  if(!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  data <- matrix$get()
  res <- solve(data)
  matrix$set.inverse(res)
  res
}

#example of invertible matrix
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8)