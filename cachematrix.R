## Put comments here that give an overall description of what your functions do

## makeCacheMatrix is a function that creates a matrix object that can store outcomes

makeCacheMatrix <- function(x = matrix()) {
    ## make an empty matrix full of NULL values
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
}
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
## cacheSolve is a funtion that computes the inverse of the data from makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## checks if this inverse is already calculated
    if(!is.null(m)) {
        ## if already calculated, show a message
        message("getting cached data")
        ## and return the previously calculated outcome
        return(m)
    }
    ## when not already calculated: get the data from x
    data <- x$get()
    ## calculate the outcome
    m <- solve(data, ...)
    x$setinverse(m)
    ## return the outcome
    m
}

