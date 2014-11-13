## Coursera Course - R Programming (rprog-009)
## Assignemtn - Programming Assignment 2
## Student - Krishnamoorthy Balaraman

## The idea is to implement an encapsulation of a matrix and its
## associated get and set methods. This is akin to implementing
## a class in an object oriented programming langiage like
## C++ or Java. The first function "makeCacheMatrix" allows the
## user to create a matrix "object" from a R matrix data variable.
## The enhancements the "matrix object" provide over the vanilla
## matrix are:
## 1. Provides get and set methods for the matrix data
## 2. Provides get and set methods for the inverse of a matrix.
## The second function cacheSolve extends the matrix object
## implementation to ensure that the value of the inverse of
## a matrix is cached upon the first invocation to return the
## inverse of a matrix. Subsequent invocations for getting the
## inverse will return the cached value. 
## Quick test: 
## x1 <- c(1,0,0)
## x2 <- c(0,1,0)
## x3 <- c(0,0,1)
## m <- rbind(x1,x2,x3)
## mat <- makeCacheMatrix(m)
## cacheSolve(mat) 
## The above invocation should return the same value as the i/p
## matrix

## Preconditions: (expectations while invoking this function)
##  a. The value of x is a valid matrix.
##  b. x is an invertible matrix
## Postconditions: (what happens after the invocation)
##  a. A list of four functions are returned
##      i. get() -> returns the matrix stored as x
##     ii. set(x) -> sets the matrix to the value of the parameter.
##    iii. getInverse() -> returns the inverse of x.
##     iv. setInverse(invX) -> Sets the inverse of x to the value of the
##                            parameter inv-x
makeCacheMatrix <- function(x = matrix()) {
    inverseOfMat <- NULL
    set <- function(m){
        x <<- m  ## Using the regular assignment operator "<-" will cause 
                 ## a new x to be created within the set function!
        inverseOfMat <- NULL
    }
    get <- function() x
    
    setInverse <- function(invX){
        inverseOfMat <<- invX
    }
    getInverse <- function() inverseOfMat
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Precondition: 
##  a. x is an object that has been constructed using makeCacheMatrix()
## Postcondition:
##  a. The inverse of the matrix x is returned to the caller
##  b. The inverse of the matrix represented by x is computed if and only if
##     it has not been computed already. This value is stored within x by
##     invoking the setInverse() method of x
##  
cacheSolve <- function(x, ...) {
    invX <- x$getInverse()
    if (!is.null(invX)){
        print("Getting Cached Data...")
        return(invX)
    }
    # First time computation of inverse. Compute and cache.
    data <- x$get()
    invX <- solve(data,...)
    x$setInverse(invX)
    invX
}
