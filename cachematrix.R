## Its puspose is to store a martix and a cached value of the inverse of the 
## matrix. Contains the following functions:
## * setMatrix      set the value of a matrix
## * getMatrix      get the value of a matrix
## * setInverse   set the cahced value (inverse of the matrix)
## * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) z <<- inv
  getinverse <- function() z
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## The following function calculates the inverse of a "special" matrix created with 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
       
  z <- x$getinverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  m <- x$get()
  z <- solve(m, ...)
  x$setinverse(z)
  z
  ## Return a matrix that is the inverse of 'x'
}
