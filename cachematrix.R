## Vishal Changrani
## vishal_changrani@yahoo.com
## Solution for programming assignment 2 - R Programming Coursera 8/17/2014
## function makeCacheMatrix acts as the cache
## function cacheSolve is the function that updates the cache and returns the inverse.

## makeCacheMatrix - Creates a vector of functions to set and get the value of a matrix and it's inverse
## Input parameter - x - The input matrix
## Return value - A vector containing four functions - set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve - Returns the inverse of the given matrix. 
## Searches the cache first for the given matrix. If not found then caculates inverse and updates cahce.
## Input parameter - x - The input vector
## Return value - The inverse of matrix x.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
	        if(!is.null(m)) {
	                message("getting cached data")
	                return(m)
	        }
	        data <- x$get()
	        m <- solve(data)
	        x$setinverse(m)
        m
}
