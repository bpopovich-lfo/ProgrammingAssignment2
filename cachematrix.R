## The below is a pair of functions that cache the inverse of a matrix.



makeCacheMatrix <- function(x = matrix()) {
    
    ## This function creates a special "matrix" object that can cache its inverse
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
## Return a list of the functions to set and get original and inverse of matrics.  
## This will be used as the input to cacheSolve
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}




cacheSolve <- function(x, ...) {
    
## This function will return a matrix that is the inverse of 'x' 
## 'x' will be output of makecachematrix()
    
    inv <- x$getinv()
    
## If matrix of the inverse is cached, recall cached data
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
## If matrix of the inverse is NOT cached, create inverse
    
    data <- x$get()
    inv <- solve(data, ...)
    
## Store/cache and then return the inverse
    
    x$setinv(inv)
    inv
}
