## The makeCacheMatrix function creates a matrix object, which can cache it's own inverse.
## The following 4 functions are part of makeCacheMatrix:
## 1. set the valuea of the matrix.
## 2. get the values of the matrix.
## 3. set the values of the inverse matrix.
## 4. get the values of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function calculates the inverse the matrix object defined by makeCacheMatrix above.
## If the inverse is already calculated and the matrix is the same, the cacheSolve gets the 
## inverse matrix from the cache.
## However if the inverse is not yet calcvulated mat_data gets the matrix stored in makeCacheMatrix,
## calculates the inverse and stores it in makeCacheMatrix via x$setmean(m).


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