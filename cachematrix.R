## two functions below the compute the inverse of the matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mat <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mat);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse matrix that was returned from above


cacheSolve <- function(mat, ...) {
    inverse <- mat$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mat$get()
    invserse <- solve(data, ...)
    mat$setinv(inverse)
    return(inverse)
}
