# --------------------------------------------------------------------------------------------------------------------
# Coursera: R programming → Week 3
# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
# --------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------------
# Write the following functions:

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.
# --------------------------------------------------------------------------------------------------------------------

## Put comments here that give an overall description of what your
## functions do

# --------------------------------------------------------------------------------------------------------------------
# makeCacheMatrix function
# we assume, the matrix provided is a square, inversible matrix
# --------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/Gyorgy Szelezsan/Documents/GitHub/ProgrammingAssignment2")
makeCacheMatrix <- function(x = matrix()) {
      ## similarly to makeVector, we set the x to be a matrix class object
      iX <- NULL 
      # this will later store the inverse of x
      
      set <- function(y) { 
            # set values to x but not to inverse (iX) yet
            x <<- y
            iX <<- NULL
      }
      
      get <- function() x 
      # get matrix "x"
      
      setinverse <- function(inverse) iX <<- inverse 
      # inverse the matrix and store it in environment "iX"
      
      getinverse <- function() iX  
      # get "iX"
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
      # function list, where each "command" equals to it's function
}

# --------------------------------------------------------------------------------------------------------------------
# cacheSolve function
# we assume, the matrix provided is a square, inversible matrix
# --------------------------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      iX <- x$getinverse()
      # gets inverse (iX) data from "x" environment
      
      if(!is.null(iX)) {
            # since we set "iX" to be NULL at the beginning, if "iX" is not null (!is.null(iX)), 
            # then we already have pre-calculated inverse
            
            message("Getting cached matrix")
            return(iX)
      }
      
      # however if "xi" is NULL, then cache was not calculated, 
      # and we need to solve the matrix
      data <- x$get()
      # get data
      
      iX <- solve(data, ...)
      # solve "data" and store it in "iX"
      
      x$setinverse(iX)
      # set the inverse
      
      iX
      # display the result
}

# --------------------------------------------------------------------------------------------------------------------
# Testing the functions
# --------------------------------------------------------------------------------------------------------------------


s <- 1:5 
s2 <- sample(s, 100, replace = TRUE) # sample from 1:5, 100 random numbers
sm <- matrix(s2, ncol = 10) # coerce into 10x10 matrix, let's hope it's inversible

sm_cache2 <- makeCacheMatrix(sm)
cacheSolve(sm_cache2)
