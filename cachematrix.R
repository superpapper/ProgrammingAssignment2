## Following functions create a special 'matrix' object that could cache the inverse of the 'matrix'

#The first function, ` makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the matrix inverse
#4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #1.  set the value of the matrix  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  #2.  get the value of the matrix
  get <- function() x
  
  #3.  set the value of the matrix inverse
  setinverse <- function(inverse) m <<- inverse
  
  #4.  get the value of the matrix inverse
  getinverse <- function() m
  
  #5.  Return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()  #looking into cache to see the inverse exist or not
  if(!is.null(m)) {    # if the cache already has value for inverse, then return that value
    message("getting cached data")
    return(m)
  }
  
        ## Solve for matrix inverse when there is nothing in the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
