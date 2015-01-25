## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The makecacheMatrix function creates a special "matrix" 
# object that can cache its inverse.
# 1 set the value of the Matrix
# 2 get the value of the Matrix
# 3 set the value of the inverse of the matrix by using solve()
# 4 get the value of the inverse of the matrix by using solve()
# how to test: 
# source("cacheMatrix.R")
# > a<- matrix(c(3,2,0,0,0,1,2,-2,1),3,3) - Matrix 3 x 3
# a
# > matrixCache <-makeCacheMatrix(a) 
# > cacheSolve(matrixCache) --- inverse matrix
# > cacheSolve(matrixCache) --- get data from cache


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}

## Write a short comment describing this function

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache.

# verify if inverted matrix already exist
# if it already exist returned inverted matrix
# else get matrix and inverted it
# return inverted matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
