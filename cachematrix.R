## Put comments here that give an overall description of what your
## functions do

## This function generates a special vector (object) with some utility functions for a Matrix passed as argument
## This allows to perform additional function calls to the matrix like caching the result for the inverse.
## Very useful if the inverse is costly to calculate 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function provides a way to calculate the inverse of a special matrix object, 
## The input for this function has to be a vector object created with makeCacheMatrix function.
## This way every time you run this function with the special object, it can calculate the inverse of the matrix if it's not calculated
## previously and save the result in the vector object, so that the following runs can use directly the cached result, very useful if 
# the operation is costly to perform
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting matrix cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
