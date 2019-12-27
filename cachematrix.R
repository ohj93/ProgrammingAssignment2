## Cache the inverse of a matrix, so that matrix inversion
## does not need repeated computing.

## Create special "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If theintevse has alredy been 
## calculated, (and the matrix has not changed), then the 
## function should retreive the inverse from cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
