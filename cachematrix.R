## Functions that can create a new object that stores a matrix and its inverse.
## It caches the inverse natrix so it can be retrieved without being recomputed,
## so it manages to use computer resources more efficiently. 

## makeCacheMatrix is a function that creates an object that contains a 
## matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a function that returns the inverse of matrix 'x'. 
## If the inverse of the matrix is cached then it retrieves it from memory
## Otherwise it computes it.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}