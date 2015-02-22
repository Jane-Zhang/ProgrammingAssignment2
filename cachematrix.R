## The two functions below are used to create a special object that stores a matrix and cache's its inverse.
## the function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.It returns to a list containint 4 functions.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {##1/4function:Set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x##2/4function:Get the value of the matrix
        setinverse <- function(inverse) inv <<- inverse##3/4function:Set the value of the matrix's inverse
        getinverse <- function() inv##4/4function:Get the value of the matrix's inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##calculates the inverse of the special "matrix" created with the above function. 
##it either directly gets the inverse from the cache(if the inverse has already been calculated)
##or without a cached inverse, it calculates it and sets the inverse value  in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {## if the inverse has already been calculated
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)##calculates it and sets the inverse value
        x$setinverse(inv)
        inv##return the inverse of input matrix
}
