##  Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly, so that when we need it again, it can be looked up in the cache rather than recomputed.  In this assignment I define two functions that are used to create a special object that stores a matrix and cache's its inverse matrix. 

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    inverse_m <- NULL
    set <- function(y) {
      x <<- y
      inverse_m <<- NULL
    }
    get <- function() x
    set_inverse_matrix <- function(inverse_matrix) inverse_m <<- inverse_matrix
    get_inverse_matrix <- function() inverse_m
    list(set = set, get = get,
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = get_inverse_matrix)
}


## The function "cacheSolve" calculates the inverse of the special matrix created with the function "makeCacheMatrix". However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse_matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the set_inverse_matrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse_m <- x$get_inverse_matrix()
    if(!is.null(inverse_m)) {
      message("getting cached data")
      return(inverse_m)
    }
    data <- x$get()
    inverse_m <- solve(data, ...)
    x$set_inverse_matrix(inverse_m)
    inverse_m
}


