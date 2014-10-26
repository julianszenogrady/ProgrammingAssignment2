## 1st function creates inverse matrix and caches it
## 2nd function checks cached results, otherwise computes the inverse matrix

## Creates inverse matrices and caches them

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() solve
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Checks wheter result is already cached, otherwise computes it

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                m
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}