## x,inv at lines 15,16 are objects in an makeCacheMatrix's environment that
## is different from the current environment i.e. set() function
## Below are two functions that are used to create a special object that stores a 
## matrix and cache's its inverse.
## The first function, makeCacheMatrix creates a special (inversable)"matrix", which 
## is really a list containing a function to

## set the value of the matrix i.e. set()
## get the value of the matrix i.e. get()
## set the value of the inverse i.e. setInv()
## get the value of the inverse i.e. getInv()

makeCacheMatrix <- function(x = matrix()) { {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL 
        }
        get <- function() x
        setInv <- function(I) inv <<- I
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
}


## cacheSolve() computes the inverse of the special "matrix" returned
## by makeCacheMatrix() If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache, 
## else it would be found and updated by calling setInv()

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
