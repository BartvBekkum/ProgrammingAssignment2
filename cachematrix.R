
makeCacheMatrix <- function(x = matrix()) {
         ## Create a vector with the functions:
         ##    set (to save the original matrix)
         ##    get (to retrieve the original matrix)
         ##    setinverse (to save the calculated inverse of the original matrix)
         ##    getinverse (to retrieve the calculated inverse)
 
   m <- NULL
   set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           ## Check if matrix x is already stored via makeCacheMatrix
           ## is so; retrieve the already calculated and stored inverse via getinverse
           ## is not so: calculate the inverse and store it via setinverse
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m}
