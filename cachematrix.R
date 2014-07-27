## makeCacheMatrix - Provides functions get, set, setinverse and getinverse
## 		   - Stores the prior value of the matrix in x
##		   - Stored the prior value of the inverse in m 
## cacheSolve - Determines if the matrix inverse has been calculated
##	      - If yes, it returns the cached value
##	      - Else, it runs a fresh computation and stores the value 



## The makeCacheMatrix function is used to cache the value of the matrix, 
## using the variable m and operator <<

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of a matrix
## If the matrix is the same, it returns the cached/originally computed value
## Else it computes the inverse from scratch

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}