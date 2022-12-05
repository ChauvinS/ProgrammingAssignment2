## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly. 
## Please find below two functions that cache the inverse of the matrix
## Asssume that the matrix supplied is always invertible

## The makeCacheMatrix function creates a special matrix object that can cache its inverse
## 1st step : set the value of the matrix
## 2nd step : get the value of the matrix
## 3rd step : set the value of the inverse of the matrix
## 4th step : get the value of the inverse of the matrix

makeCacheMatrix <- function (x = matrix()) {
  
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function () x  
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## The cacheSolve function computes the inverse of the special matrix returned by the function above
## If the inverse has already been calculated (and the matrix has not changed),
## Then the following function retrieves the inverse of the matrix from the cache
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache
## via the setinverse function

cacheSolve <- function(x, ...) {
  
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)       
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
