## Create a special object that stores a numeric vector and cache's its mean
##

## Create a special object that stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x

  setinverse <- function(inv) inverse <<- inv

  getinverse <- function() inverse

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
