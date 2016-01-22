## This function makes a list that can set and get
##the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
     
     matinv <- NULL
     set <- function(y) {
          x <<- y
          matinv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) matinv <<- inverse
     getinverse <- function() matinv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function returns the inverse of a matrix. To save time it checks 
## to see if inverse has been computed already. If not it stores the inverse
## in the cache. 

cacheSolve <- function(x, ...) {
     matinv <- x$getinverse()
     if(!is.null(matinv)) {
          message("getting cached data")
          return(matinv)
     }
     data <- x$get()
     matinv <- solve(data, ...)
     x$setinverse(matinv)
     matinv
}
