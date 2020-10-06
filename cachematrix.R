## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#function to cahce the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
  }

#print(makeCacheMatrix())

## Write a short comment describing this function
#checks to see if it is the inverse. If it gets the inverse from the cache, skips calc.
# If it doesnt, it creates the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- (x$getinv())
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setinv(inv)
  inv

}
