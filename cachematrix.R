## Program for caching the inverse of a matrix
##
## Given the fact that calculating the inverse of a matrix
## can result in a costly computation, caching the inverse 
## of the matrix gives some benefits in avoiding  repeated
## calculation. The two following functions cache the inverse
## of a matrix.

## Given a matrix x, create and return a 'special' matrix 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Given a matrix object x, returned by makeCacheMatrix(),
## get the cached inversion of it if it is already calculated,
## or calculate its inversion and cache it. Return the inverse
## of the inputted matrix

cacheSolve <- function(x=matrix(), ...) {
        
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
