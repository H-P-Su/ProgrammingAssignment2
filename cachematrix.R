## Put comments here that give an overall description of what your
## functions do

# Based on the template, makeVector, this function should be a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
  }
  get <- function() x
  setinverse <- function(inverse_matrix) i<<- inverse_matrix
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


# this function should see if the inverse has been set.
# if the inverse has been set, it should get the inverse matrix
# if the inverse has not been set, it should set the inverse matrix with the setinverse function.
# assuming the matrix supplied is always invertable

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("Getting cached inverse.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

# Test Cases:
# m1 <- matrix(c(1,2,3,2,2,6,2,3,4), 3, 3)
# m2 <- matrix(c(1,2,2,1,4,2,3,2,1), 3, 3)

# test <- makeCacheMatrix(m1)
# cacheSolve(test)
