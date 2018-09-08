## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Caches the Inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
       minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minv <<- inverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If Inverse is already present in Cache, that is returned.
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached inverse")
                return(minv)
        }
        invdata <- x$get()
        minv <- solve(invdata, ...)
        x$setinverse(minv)
        minv
}
