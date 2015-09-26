## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function has your sub fxn for settng a metrix, getting a matrix, setting inverse of the matrix and getting that inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL								## define inverse as an empty matrix
        set <- function(y) {					## set function to set a new matrix and make inverse null for previous matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x						## print the matrix
        setinverse <- function(inv) i <<- inv	## set function to set inverse of matrix
        getinverse <- function() i				## print the inverse
        list(set = set, get = get,				## return list
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This fxn checks if the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()						## get inverse matrix
        if(!is.null(i)) {						## check if inverse is null if not return inverse
        		message("getting cached data")
                return(i)
        }
        data <- x$get()							## get the matrix from our object
        i <- solve(data, ...)					## calculate inverse of the matrix
        x$setinverse(i)							## returning inverse matrix
        i
}
