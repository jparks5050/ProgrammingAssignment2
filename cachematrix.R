## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        
       ## The following code checks the cache to determine if the inverse has
       ## already been calculated
       
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
       
       ## The following code calculates the inverse for the matrix after it is determined
       ## that the data was not available in the cache.
       
        data <- x$get() 
        m <- solve(data, ...)
        x$setsolve(m) ## inverse is now cached for later calculations
        m
}
