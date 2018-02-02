## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #clear any inv values
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #get a cached value for inv
        if(!is.null(inv)) { 
                message("getting cached data")
                return(inv) #if there is a cached value, return this 
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
