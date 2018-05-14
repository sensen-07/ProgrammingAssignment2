makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
mat <- makeCacheMatrix()
mat$set(matrix(data = (1:10), nrow = 5, ncol = 2))
mat$get()

cacheSolve <- function(x) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
mat <- makeCacheMatrix()
mat$set(matrix(data=(1:6),nrow = 2, ncol = 3))
mat$get()
mat$getinverse()
cacheSolve(mat)
mat$getinverse()
cacheSolve(mat)