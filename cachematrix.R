## This function accepts a matrix as input,
## and creates a list containing 4 functions,
## which perform these actions:
## a space called m is emptied
## set: this function makes a copy of the matrix and puts it in a space called x.
## get: this function makes a copy of the matrix stored in x, and puts it in space $get.
## setinverted: this function puts its content (inverted matrix or nothing) in a space called m
## getinverted: this function copies the content of space m and stores it in $getinverted

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) m <<- inverted
  getinverted <- function() m
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}

## The function cacheSolve accepts the output of function makeCacheMatrix as input,
## and checks if $getinverted has content.
## if yes, it delivers the content of $getinverted (=inverted matrix) as output
## if no, it uses the matrix from space $get to calculate an inverted matrix,
## and delivers this as output. 


## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$getinverted()
    if(!is.null(m)) {
      message("getting cached inverted matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
  }

