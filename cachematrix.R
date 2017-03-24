## Put comments here that give an overall description of what your
#In this program, we are cathing the inverse of a matrix 
#since it is somehow costly perfoming matrix inversion
## The makeCacheMatrix creates a list containing a function to
#a.Set the value of the matrix
#b.Get the value of the matrix
#c.set the value of inverse of matrix 
#d.Get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Write a short comment describing this function
# This function returns the inverse of the matrix. It firstly 
#checks if the inverse haS already been computed. If so,
#it gets the result and skips the computation. If that
#isn't the case, then, it computes the inverse and it sets 
#the value in the cache through setinverse function.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
       
}

