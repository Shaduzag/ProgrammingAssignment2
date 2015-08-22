## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get  <- function() x
    setinverse  <- function(i) inverse  <<- i
    getinverse  <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }


## Create the inverse. If the inverse already been calculated,then retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    inverse  <- x$getinverse()
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data  <- x$get()
    inverse  <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
