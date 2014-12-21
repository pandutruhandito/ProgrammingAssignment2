## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    list(get=get,setinv=setinv,getinv=getinv)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
