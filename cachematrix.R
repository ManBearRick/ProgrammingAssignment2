## With this assignment, the goal is to write a pair of functions named 
## "makeCacheMatrix" and "cacheSolve" that cache a matrix's inverse.     

## makeCacheMatrix is a function which creates a special matrix object that can
## cache its inverse for what is input.

## *note, it must be a square matrix.

makeCacheMatrix <- function(x = numeric()) {
    cache <- NULL
    setMatrix <- function(newValue) {
        x <<- newValue
        cache <<- NULL
    }
    getMatrix <- function() {
        x
    }
    cacheInverse <- function(solve) {
        cache <<- solve
    }
    getInverse <- function() {
        cache
    }
    list(setMatrix = setMatrix, 
         
         getMatrix = getMatrix, 
         
         cacheInverse = cacheInverse, 
         
         getInverse = getInverse)
}

## "cacheSolve" is a function that gives the inverse of the special "matrix" 
## returned by "makeCacheMatrix". If the inverse has already been calculated 
## then "cacheSolve" will get the inverse from the cache.

cacheSolve <- function(y, ...) {
    inverse <- y$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- y$getMatrix()
    
    inverse <- solve(data)
    
    y$cacheInverse(inverse)
    
    inverse
}

## Checking that the program works.

## my_matrix <- matrix(1:4, 2, 2)
## my_matrix1 <- makeCacheMatrix(my_matrix)
## cacheSolve(my_matrix1)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5