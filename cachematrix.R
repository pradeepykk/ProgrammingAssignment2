## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL									#define inverse as an empty matrix
        set <- function(y) {						#set function to set a new matrix and make inverse null for previous matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x							#print the matrix
        setinverse <- function(inv) i <<- inv	#
        getinverse <- function() i					#print the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
        		  message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
