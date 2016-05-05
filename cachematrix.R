## The purpose of these function is to cache time-consuming linear algebra calculation ( ie, finding the inverse)
## most of this process is borrowed from the instruction provided in the assignment

## this functin will create a list of function to set and get a matrix x
## it will also set and get the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) { x <<- y ;  m <<- NULL }
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  }


## Find the inverse of a matrix x, if and only if, it does not already exist

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { message("getting cached data") ; return(m) }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m                            }


# test case
# x <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow = 3, ncol = 3, byrow=TRUE)
# m <- makeCacheMatrix(x)
# m <- cacheSolve(m)
# m

