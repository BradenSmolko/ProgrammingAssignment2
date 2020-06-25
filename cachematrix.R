## These functions allow users to create a special "matrix" object that is actually a list of functions that allow
## for getters and setters of a matrix cache to be accessed. Basically you can create and store a matrix and it's inverse
## in the parent scope and access them later with other functions that take a "makeCacheMatrix" type of list object.


## This function will make a special cached matrix object that will be a list of functions that
## will allow the user to get or set the matrix (x) or the inverse (invMat) for the following function

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        setMat <- function(y) {
                invMat <<- NULL
                x <<- y
        }
        getMat <- function() x
        setInv <- function(y) invMat <<- y
        getInv <- function() invMat
        
        list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## This function will retrieve the cached inverse if one exists, otherwise it will solve the matrix
## and then store it in the cache before retrieving it.

cacheSolve <- function(x, ...) {
        invMat <- x$getInv()
        if(is.null(invMat)){
                x$setInv(solve(x$getMat()))
        }
        message("Getting cached Inverted Matrix")
        x$getInv()
}
