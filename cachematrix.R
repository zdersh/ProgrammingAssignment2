## These functions compute the inverse of a matrix while caching the result
## When the inverse of a specific matrix is requested the cache is consulted to check
## if a cached result exists.  If so, it is returned instead of recalculation.  If it 
## not in the cache it is calculated and the result is placed in the cache for future use.

## This function enables access to a cache of matricies and their calculated inverse values.
## the cache is stored in a non local environment so that it is persistent

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function tries to read a cached result.  If it exists, it is returned without further calculation
## If no result exists in the cache, it is calculated and placed in the cache for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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
