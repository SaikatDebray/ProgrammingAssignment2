## makeCacheMatrix basically creates a function which facilitates in creating a matrix
## it allows to set the matrix and get the matrix. This fucntion also allows to set the inverse
## and get the inverse.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function resolves the inverse of the matix generated in the previous function.
## the assumption is, that the matrix generated in the above function is a square matrix
## i.e it has equal number of rows and col.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## use case to test the functions is 

##mc <- matrix(rnorm(16), 4,4, byrow = TRUE)
##mc1 <- makeCacheMatrix(mc)
##cacheSolve(mc1)
