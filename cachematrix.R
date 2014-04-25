## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
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


## This functin, 'cacheSolve' takes the output of the first function as input and tries to 
## ouput the inverse of the matrix.
## first, it checks whether the inverse has already been calculated and stored in the cache.
## if yes, it skips the computation and retrieve the inverse from the cache.
## if not, if calculates the inverse and store it in the cache.


cacheSolve <- function(x, ...) {
    m <- x$getinverse()  ## retrieve the inverse from the cache
    if(!is.null(m)) {  ## test whether the retrieved inverse from the cache is NULL
        message("getting cached data") ## if is not null, print the message 
        return(m)                      ## and return the retrieved inverse
    }
    data <- x$get() ## if the inverse is not in the cache, get the matrix from the cache
    size <- dim(data)[1]  ## get the size of the matrix
    m <- solve(data, diag(size)) ## calculate the inverse of the matrix 
    x$setinverse(m)  ## store it in the cache
    m   ## return the inverse
}
