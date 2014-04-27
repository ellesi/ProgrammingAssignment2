## Function to cache the inverse of a matrix.

## This function creates a "matrix" object, which is really a list 
## containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <- NULL
    }
    get <- function() x
    setinv <- function(solve) Inv <<- solve
    getinv <- function() Inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the "matrix" created with the 
## above function. If the inverse has already been calulated it gets it from
## the cache and skips the computation. Otherwise, it callculates it and sets
## it in the cache.

cacheSolve <- function(x, ...) {
    Inv <- x$getinv()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setinv(Inv)
    ## Return a matrix that is the inverse of 'x'
    Inv    
}
