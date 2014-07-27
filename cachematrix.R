## These functions caches the inverse of the matrix to prevent the recomputation 
## for the subsequent calls of the inverse for the same matrix

## Caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
  x <<- y
  i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns cached inverse and if not cached then computes it and cache it for the given matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
