## The functions below allow to cache the inverse of the matrix if the matrix
## has been already created and its elements have not been changed.

## The first function makeCacheMatrix creates a matrix object that can cache
## its corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The second function returns the inverse of matrix object created by
## makeCacheMatrix above. If the matrix has not changed the function just
## retrieves the inverse from cache.

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}