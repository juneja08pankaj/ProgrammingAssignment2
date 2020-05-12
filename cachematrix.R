## Folowing two functions (in conjunction) can be used to cache the inversion of a matrix
## functions do

## makeCacheMatrix is used to get or set(caching) a matrix, which can further be used to set(caching)\get the inversion of a matrix

## n4<-makeCacheMatrix()
## n4$set(matrix(1:4,2,2))
## n4$get()
## n4$setinversion(solve(matrix(1:4,2,2)))
## n4$getinversion()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) i <<- inversion
  getinversion <- function() i
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## follwing function can be used to return the inversion of a matrix, in case the result is already cached, it would be returned from cache 
## otherwise will be caculated and return (also the result would be cached, so that it can returned from cache next time)

## n5<-makeCacheMatrix()
## n5$set(matrix(2:5,2,2))
## cacheSolve(n5)

cacheSolve <- function(m, ...) {
  i <- m$getinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setinversion(i)
  i
}
