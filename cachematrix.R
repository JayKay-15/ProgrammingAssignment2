## Put comments here that give an overall description of what your
## functions do

## Week 3 assignment 3/13/2021
## This assignment will produce a pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## initialize inverse as NULL
    set <- function(y) {                    
        x <<- y                             
        inv <<- NULL                        
    }
    get <- function() x                     ## function to get maxtrix x
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inverse
    getinverse <- function() inv                     ## gets the value of inverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) 
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {             
        message("getting cached data")              ## checker to see if inverse is null
        return(inv)                                 ## returns inverse value
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv                                             ## returns matrix that is inverse of x
}



