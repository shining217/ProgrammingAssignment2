## This forms a pair of functions with cacheSolve to cache the inverse
## of a matrix to illustrate the value of being able to cache
## a calculation.

## This function creates a list that sets up the conditions 
## needed for cacheSolve to work. It stores any previously
## calculated inverse matrix in setinv. This is passed when 
## cacheSolve is run.

makeCacheMatrix <- function (x = matrix()) {
     m <- NULL   # creates m and sets to NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x    # get will return the matrix using m$get

     ## now create the set and get functions
     setinv <- function(solve) m <<- solve 
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## This function will create the inverse matrix of the matrix
## using the list created in makeCacheMatrix
## Importantly it will ONLY do this if the inverse matrix
## has not already been calculated.
cacheSolve <- function(x, ...) {
     m <- x$getinv()
     
     ## Check to see if the calc has already been performed
     ## If so then is.null(m) is FALSE, so !is.null(m) is TRUE
     ## and message is shown
     if(!is.null(m)) {
          message("Calculation already performed - getting cached data")
          return(m)
     }
     
     ## Otherwise pass the makeCacheMatrix$get into data
     data <- x$get()
     
     ## pass the result of running the solve function on the matrix into m
     m <- solve(data)
     
     ## set the value of m in the parent environment
     ## in makeCacheMatrix so that the test (if(!is.null(m)))
     ## will return TRUE if run before x is reset.
     x$setinv(m)
     
     ## Return m
     m
}
