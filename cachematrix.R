## Similar with the makeVector except that in this function, assuming that the matrix (x) is inversible,
## this code caches the inverse of a matrix.

## This function will 'set' and 'get' the input matrix to be inversed.
## Then, 'setinv' will set the inversed matrix to some local variable i. Basically caching the inverse.
## While 'getinv' will sort of extract/get the inverse from the cache variable i
## 'set', 'get', 'setinv', and 'getinv' are functions

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This is a function that will check whether there is a cached inverse.
## If there is, this will simply return the cached data. Otherwise,
## it will get the inverse then return it.

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
