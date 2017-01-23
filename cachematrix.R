## Put comments here that give an overall description of what your
## functions do

## The functions below are used to compute the inverse of a matrix.
## Please note the matrix in question is considered a special matrix,
## and one can call makeCacheMatrix to call its appropiate getters
## and setters. CacheSolve computes the inverse, and optimizes itself,
## by looking in the cache first, and if not present computing the inverse


## Write a short comment describing this function

## This method computes a special matrix which has the potential
## to cache its own inverse. It exposes a series of functions
## like setinverse, getinverse, which help as getters and setters

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        getinverse <- function() inv
        setinverse <- function(inverse) inv <<- inverse
        list(set = set, get = get,
             getinverse = getinverse,
             setinverse = setinverse)
}


## Write a short comment describing this function

## This function is used to compute the inverse of a matrix
## This function optimizes itself, so if a matrix's inverse
## hasnt been computed, it computes it, else it loads from
## its cache.
## args: x <- the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
            return(inv)
        }
        else{
            inv <- solve(x$get())
            x$setinverse(inv)
            return(inv)
       }
}
