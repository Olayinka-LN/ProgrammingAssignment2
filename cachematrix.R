## This assignment is to create a pair of function that can create a special 
## matrix and at the same time cache the inverse of the matrix.


## The first function "makeCacheMatrix" is to create the special "matrix" object 
## that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The second function below "cacheSolve" is to compute the inverse of the special 
## martix returned by the makeCacheMatrix function above. If the inverse has already 
## been calculated (on the same matrix), the cacheSolve should retrieve the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
        
}