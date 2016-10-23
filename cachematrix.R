##The following fuctions allow you to a) create and access a matrix object
##that can cache its inverse and b) calculate the inverse or retrieve it from the cache(if it already exists) 

## makeCacheMatrix takes a matrix as it argument. It then has functions to 
## store and retrieve the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## When called cacheSolve will try to retrieve the cached value of the matrix inverse. If 
## it does not exist, it calculates the value and caches it for future use.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

