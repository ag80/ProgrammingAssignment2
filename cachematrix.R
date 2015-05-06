## makeCacheMatrix is a function that takes a matrix as input, and stores 
## a list of four functions relating to that matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  ## the inverse is initially NULL
      
      set <- function(newmatrix) {   # set will update with a new matrix
            x <<- newmatrix         # and reset the inverse to NULL
            inv <<- NULL
      }
      
      get <- function() x  # get simply returns the matrix
      
      # setinverse passes the results of another function (solve), and 
      # assigns inv to the new value.
      setinverse <- function(inversion) inv <<- inversion
      # getinverse will return the inverted matrix
      getinverse <- function() inv
      
      # the return of makeCacheMatrix is a list of these functions
      list(set = set, get = get,
            setinverse = setinverse, getinverse = getinverse)
}
      
      
## cacheSolve computes the inverse of a matrix, sets the value for the calling
## function, and stores the result in memory for retreival later

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv

      
}
