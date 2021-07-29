## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to
# 
#  - set the value of the matrix
#  - get the value of the matrix
#  - set the value of the inverse
#  - get the value of the inverse
#     (Adapted from the coursera description of cachemean for this excercise)
makeCacheMatrix <- function(x = matrix()) {
    # initialize the matrix to NULL
    inverse  <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    
    setinverse <- function(inv) { 
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    } 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in
# the cache via the setinverse function.
#     (Adapted from the coursera description of cachemean for this excercise)
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
