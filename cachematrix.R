## Two functions here
## makeCacheMatrix sets a matrix into the cache
## cacheSolve returns the inverse of a matrix

## makeCacheMatrix takes in matrix x and stores in cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    i <<- inverse
  }

  getInverse <- function() {
    i
  }
  
  list(
    set = set, 
    get = get, 
    setInverse = setInverse, 
    getInverse = getInverse
    )
}


## Either returns the inverse of the cached matrix for x or returns the matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    if(!is.null(i)) { ## getting cache
      return(i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)

    x$setInverse(i)

    i  ## returns the inverse matrix
}
