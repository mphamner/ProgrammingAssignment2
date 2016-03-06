## This is R Programming Week 3 Assignment 2 to write 2 functions
## that will cache the inverse of a special matrix rather than
## requiring that it be recomputed - even when that is not
## necessary. 

## This function will create a special matrix that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special matrix
## created by makeCacheMatrix.  This function first checks
## to see if the matrix has changed.  If not and the inverse
## has already been computed then it will retrieve the 
## inverse from cache rather than go through the computation
## again.  

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

