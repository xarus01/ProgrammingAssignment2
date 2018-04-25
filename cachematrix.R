## Put comments here that give an overall description of what your
## functions do
## makeCachMatrix will creates matrix that can cache inversed version of itself
## cacheSolve will either compute the inverse of matrix or return cached version if it was already computed


## Write a short comment describing this function
## makeCache returns list object with get, set, getInverse, and setInverse function
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y){
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()

    ## If matrix was already computed(aka cached), return it
    if(!is.null(inv)){
        return(inv)
    }

    ## Inverse matrix x and cache the value
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)

    ## Return inverse matrix
    inv
}
