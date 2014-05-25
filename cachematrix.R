## This script contain 2 functions 
##makeCacheMatrix() which accepts a matrix arg and creates an object which contains list of set and get functions.
## cacheSolve() calculates the inverse of a matrix, if called for a same input retruns 
## the cached data, without computation

## Returns a special object which is list of 4 functions coded in this function
##

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


## cacheSolve function takes the input argument, which is renamed to x temporarily for the
## for the scope of function and calculates Inverse by calling solve(), and provides 
## functionality of getting cached data

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
