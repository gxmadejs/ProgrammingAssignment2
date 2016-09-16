## The following functions can be used to cache the inverse of a matrix
## to save some computational time


##makeCacheMatrix is a function that does the following
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse of the matrix
## gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
          x <<- y
          m<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <-function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## CacheSolve finds the inverse of a matrix.
## If the inverse has already been computed it returns this result.
## If it has not been computed in finds the inverse and then sets the
## value in the cache.

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
