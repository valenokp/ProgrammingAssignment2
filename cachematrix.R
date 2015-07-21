## create a list object whose elements are functions 
## to set and get matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv_x <<- inv
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## calculate or get cached inverse of makeCacheMatrix(x)
cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)
        inv_x        
}
