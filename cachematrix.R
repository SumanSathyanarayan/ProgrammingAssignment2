## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## initialize minv to NULL, that holds the value of the matrix inverse
    minv <- NULL                            
    set <- function(y) {
      ## get value of matrix from the parent environment.
      x <<- y 
      ## reset inv to NULL
      minv <<- NULL
    }
    ## return value of matrix argument
    get <- function() x 
    
    setmatrix <- function(inverse) minv <<- inverse ## assign value of minv in parent env
    getmatrix <- function() minv                    ## get the value of minv
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix
## returned by makeCacheMatrix above. If inverse is calculated already, 
## cacheSolve will get the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getmatrix()
        if(!is.null(minv)) {
          message("getting cached data")
          return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)  ## use solve function to inverse the matrix
        x$setmatrix(minv)
        minv                      ## return the inverted matrix
  
}
