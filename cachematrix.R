## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cached.result <- NULL
  set <- function(y) {
    x <<- y
    cached.result <<- NULL
  }
  get <- function() x
  set.cached.result <- function(result) cached.result <<- result
  get.cached.result <- function() cached.result
  list(set = set, get = get,
       set.cached.result = set.cached.result,
       get.cached.result = get.cached.result)
}


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
  res <- matrix$get.cached.result()
  if(!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  data <- matrix$get()
  res <- solve(data)
  matrix$set.cached.result(res)
  res
}

#example of invertible matrix
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8)