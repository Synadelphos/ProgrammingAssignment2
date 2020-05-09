## Creates a special matrix which is a list containing functions that
## set and get the value of the matrix, set and get the value of the inverse

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


##  calculates the inversion of a matrix.
##This will first check the cache to see if it's already been calculated and if so it gets the inverse from the cache
##if there is no cache already existing, it calculates the inverse and sets it to the setinverse function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("retrieving cached solution")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

