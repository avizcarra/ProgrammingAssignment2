
## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. These functions will 
## cache the inverse of a matrix 


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    print(environment())
    evn <- environment()
    print(parent.env(evn))
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. The function  assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
        }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


## USAGE
## > matrix1 <- matrix(data = c(7,12,5,8,6,8,9,10,78), nrow = 3, ncol = 3)
## > matrix2 <- makeCacheMatrix(matrix1)
## > cacheSolve(matrix2)
## [,1]        [,2]         [,3]
## [1,]  0.48379052 -0.49376559  0.007481297
## [2,] -0.10723192  0.31047382 -0.027431421
## [3,] -0.03241895  0.01246883  0.014962594


