## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Assign the value of NULL to the object 'i'
    ## to initialize it as an object within the makeCacheMatrix() environment
    ## to be used by later code in the function
    i <- NULL
    ## set the matrix 'x'
    set <- function(y) {
        ## Assign the input argument to the x object in the parent environment
        x <<- y
        ## Assign the value of NULL to the object 'i' in the parent environment.
        ## This line of code clears any value of 'i'
        ## that had been cached by a prior execution of cacheSolve()
        i <<- NULL
    }
    ## Get the matrix 'x'
    get <- function() {
        x
    }
    ## Set the inverse 'i' of the matrix 'x'
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ##  Get the inverse 'i' of the matrix 'x'
    getInverse <- function() {
        i
    }
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse 'i' of matrix 'x'
    i <- x$getInverse()
    ## Just return the inverse 'i' if it's already set
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Get the matrix 'x'
    data <- x$get()
    ## the inverse 'i' is computed by solve(x)
    i <- solve(data, ...)
    ## Set the inverse 'to the object'i'
    x$setInverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}

## test1:
## A <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)
## A1 <- makeCacheMatrix(A)
## cacheSolve(A1)
##      [,1]  [,2]
#[1,]   -9  8.75
#[2,]    8 -7.50

## test2:
## B <- matrix(c(7, -1, -1, -3, 1, 0, -3, 0, 1), 3, 3)
## B1 <- makeCacheMatrix(B)
## cacheSolve(B1)
##      [,1] [,2] [,3]
#[1,]    1    3    3
#[2,]    1    4    3
#[3,]    1    3    4
