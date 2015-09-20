# The following two functions work hand in hand to compute the 
# inverse of a matrix and cache that inverse rather than compute
# it repeatedly.

# makeCacheMatrix is a function that contains a list of 4 other functions.
#      "set" sets the value of the matrix
#      "get" retrieves the value of the matrix
#      "setsolve" sets the value of inverse of the matrix
#      "getsolve" retrieves the value of the inverse of the matrix
# The variable i is the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve calculates the inverse of the matrix (variable "i").
# If the inverse has already been calculated, it will retrieve the inverse
# from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

# Here is sample code of a randomly generated 5x5 matrix.

a <- makeCacheMatrix(x=matrix(rnorm(25), 5, 5))
cacheSolve(a)
