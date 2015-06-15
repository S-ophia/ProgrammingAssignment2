## Put comments here that give an overall description of what your
## Below are two functions that are used to create a special matrix
## and caches its inverse

## Write a short comment describing this function
# makeCacheMatrix creates a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
# The following function calulates the inverse of the special "matrix" created 
# with the above function. It first checks if the inverse has already been 
# culculated, if so,it gets the inverse from the cache and skip the computation,
# otherwise, it calculates the inverse and sets the value of the inverse in the 
# cache via the setinverse function.
cacheSolve <- function(x, ...) {
    invs <- x$getinverse()
    if(!is.null(invs)) {
      message("getting cached data.")
      return(invs)
    }
    data <- x$get()
    invs <- solve(data)
    x$setinverse(invs)
    invs
}


# Test Output Sample 
# x<-rbind(c(1,2,3),c(4,5,5),c(7,8,9))
# m <- makeCacheMatrix(x)
# m$get()
# cacheSolve(m)

