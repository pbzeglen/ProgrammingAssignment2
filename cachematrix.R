## These two functions together take a matrix and give its inverse,
## While caching that inverse for later. If asked for the inverse of a matix
## that has already been solved, it returns the chached solution.

## makeCacheMatrix takes the matrix given, and caches two variables
## x, the matrix, and y, the solution.
## creates a list of four functions:
## 1) set: sets the cached x matrix to the parameter y
## 2) get: gets the matrix x from cache
## 3) setsolve: sets the inverse m to the parameter solve
## 4) getsolve: returns the inverse m

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

## cacheSolve takes paramater x, the list created by makeCacheMatrix
## it first looks for a cached inverse solution using getSolve()
## if it finds one, it returns the cached inverse m
## if it does not find one, it calculates the inverse 
## and caches the result with setSolve, returning the solution m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
