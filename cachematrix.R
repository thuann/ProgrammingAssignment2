## author:  Thuan

## These set of functions will 
##   a) creates a special, cacheable matrix 
##   b) solves for the matrix inverse and cache for future reuse

## function: makeCacheMatrix
## desc: creates a special matrix with functions to cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrixInv <- function(matrixInv) m <<- matrixInv
    getMatrixInv <- function() m
    list(set = set, get = get,
         setMatrixInv = setMatrixInv,
         getMatrixInv = getMatrixInv)
}


## function: makeCacheMatrix
## desc: get the matrix inverse from cache if available; 
##       otherwise, solves for the matrix inverse and stores in cache for future use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getMatrixInv()
    if(!is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setMatrixInv(m)
    
    m
}
