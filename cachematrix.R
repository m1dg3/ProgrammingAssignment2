## These functions are used to calculate and cache the 
## inverse of an input matrix or retrieve a cached version
## if it exists

## makeCacheMatrix creates a list of functions which
## cacheSolve uses to calculate, cache or retrieve the
## inverse matrix of the input argument to makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    i
}


## cacheSolve calculates the inverse of the input matrix and stores it
## in 'i' or retrieves 'i' if it holds a cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("caching data")
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
