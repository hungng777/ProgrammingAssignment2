## This programming assignment writes an R function is able to cache potentially
## time-consuming computations. For example, taking the inverse of a matrix 
## is typically a slow operation, especially if it has to be computed repeatedly 
## (e.g. in a loop). If the contents of a matrix are not changing, it may make sense to ]
## cache the value of the inverse so that when we need it again, it can be looked up in 
## the cache rather than recomputed. In this Programming Assignment will take advantage 
## of the scoping rules of the R language and how they can be manipulated to preserve state 
## inside of an R object.

## This function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
##		1. set the value of the matrix
## 		2. get the value of the matrix
##		3. set the value of the inverse
##		4. get the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
    	}
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv function.
## 
## How to run the function:
## > m = matrix(1:4, nrow =2 , ncol = 2)
## > cacheSolve(makeCacheMatrix(m))
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
