## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. The funtions below will calculate and cache
## the inverse of a matrix

## This function creates a list containing functions that
##1. Set the value of the matrix
##2. Get the value of the matrix
##3. Set the value of the inverse
##4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## Inverse is NULL initially
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setminv <- function(minv) i <<- minv ## Inverse is now assigned the value
    getminv <- function() i
    ## returning list of functions
    list(set = set, get = get, setminv = setminv, getminv = getminv)
   
}


## The following function calculates the inverse of the matrix. It 
## first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via the setminv function. The function assumes
## that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getminv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    
    }
    data <- x$get()
    i <- solve(data, ...) ## calculating inverse
    x$setminv(i)
    i
    
}
