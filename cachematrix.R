## Function, makeCacheMatrix creates a list containing a function to
set the value of the matrix 
get the value of the matrix 
set the value of the matrix (inverse)
get the value of the matrix (inverse)

makeCacheMatrix <- function(x =matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinmatrix <- function(inmatrix) m <<- inmatrix
        getinmatrix <- function() m
        list(set = set, get = get,
             setinmatrix = setinmatrix,
             getinmatrix = getinmatrix)
}


##  The following function calculates the inverse matrix. It
 ##checks to see if the inverse has already been calculated (if so, it skips calculations). 

cacheSolve <- function(x, ...) {
        m <- x$getinmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinmatrix(m)
        m
}