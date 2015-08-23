## Functions that cache the inverse of a matrix
## Usage example:
##
## > source('matrixinverse.R')
## > m <- makeMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cachematrix(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cachematrix <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("cached matrix is")
    return(m)
  }
  m <- x$get()
  m <- solve(m, ...)
  x$setinverse(m)
  m
}
