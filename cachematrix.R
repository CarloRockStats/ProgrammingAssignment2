## Coursera Data Science Specialization
## Johns Hopkins University
## Course 2: R Programming
## Week 3
## Programming Assignment 2: Lexical Scoping
## Author: caeparraro@unal.edu.co
## Date: 2020-08-17



## We need to create two functions that allow us to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
## This function builds a set of functions and return the functions within a list
## to the parent environment

makeCacheMatrix <- function(x = matrix()) {
    # Initialization of objects x and m_i
    m_i <- NULL
    
    set <- function(y) {
        x <<- y
        m_i <- NULL
    }
    
    get <- function() x
    
    set_m_inverse <- function(m_inverse) m_i <<- m_inverse
    
    get_m_inverse <- function() m_i
    
    list(set = set, get = get,
         set_m_inverse = set_m_inverse,
         get_m_inverse = get_m_inverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_i <- x$get_m_inverse()
    if(!is.null(m_i)) {
        message("getting cached data")
        return(m_i)
    }
    data <- x$get()
    m_i <- solve(data, ...)
    x$set_m_inverse(m_i)
    m_i
}