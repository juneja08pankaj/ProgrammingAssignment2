## Folowing two functions (in conjunction) can be used to calculate the inverse of a matrix (when called for the first time for a given input parameter) 
## and cache the result so that during the sebsequent calls for the same input matrix, cached result will be returned in place of calculating the same.


## makeCacheMatrix is used to store the matrix in the cache and the same can be retireved from cache. 
## This Function is also capable of storing the inverse of a matrix, which can retireved from cache later.

## Example
## n4<-makeCacheMatrix()
## n4$set(matrix(1:4,2,2))
## n4$get()
## n4$setinverse(solve(matrix(1:4,2,2)))
## n4$getinverse()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## follwing function can be used to return the inverse of a matrix, in case the result is already cached, it would be returned from cache 
## otherwise will be caculated and return (also the result would be cached, so that it can returned from cache next time)

## n5<-makeCacheMatrix()
## n5$set(matrix(2:5,2,2))
## cacheSolve(n5)

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setinverse(i)
  i
}
