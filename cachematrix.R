## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## This script generates a pair of functions that cache the inverse of a matrix.
## 

## Create a list that contains a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse matrix
## 4 get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL
  set <- function(y) {
    x <<- y
 ##   inv <<- matrix(,nrow=nrow(x),ncol=ncol(x))
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
  
  }


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
  
  }
