## set the inverse of the matrix
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
       x <<- y
       i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## The following function calculates the inverse of the matrix created with the
## above function. If the inverse has not been calculated, then it should 
## calculates the inverse of the matric and set the inverse of the matrix in the
##cache via cacheSolve.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get() 
    i <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
    x$setinverse(i)
    i
}


