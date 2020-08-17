## We need to create two functions that allow us to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
## This function builds a set of functions and return the functions within a list
## to the parent environmetn

makeCacheMatrix <- function(x = matrix()) {
    # Initialization of objects x and m_i
    m_i <- NULL
    
    set <- function(y) {
        x <<- y
        m_i <- NULL
    }
    
    get <- function() x
    
    set_m_inverse <- function(m_inverse) m_i<<- m_inverse
    
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


a <- matrix(data = c(2, 7, 19, 11, 3, 5, 1, 3, 46), nrow = 3)
solve(a)

aMatrix <- makeCacheMatrix(a)
aMatrix$get()
aMatrix$get_m_inverse()
cacheSolve(aMatrix)
aMatrix$get_m_inverse()
