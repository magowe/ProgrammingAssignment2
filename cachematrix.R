# Matrix inversion is usually hard to compute and there are some benefits
# to caching the inverse of a matrix instead of computing it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# This makeCacheMatrix function below creates a list which contains a function to:
# - set the value of the matrix,
# - get the value of the matrix,
# - set the value of inverse of the matrix,
# - get the value of inverse of the matrix,
makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) iv <<- inverse
    getinverse <- function() iv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function below returns the inverse of the matrix. Firstly it checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse and sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    iv <- x$getinverse()
    if(!is.null(iv)) {
        message("I'm getting cached data now.")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data)
    x$setinverse(iv)
    iv
}
