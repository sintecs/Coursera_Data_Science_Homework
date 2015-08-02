##------------------------------------------------------------
## Assignment 2
## These functions can be used to create a matrix and
## store the matrix and its inverse in a "cache"
## This is useful in case the matrix and its inverse
## need to be used multiple times as the cache of the
## inverse negates requiring calculation of the inverse
## on each use and instead uses the pre-computed, "cached"
## copy

## Example usage:
## myMat <- matrix(c(1, 1, -3, 0, 1, 0, 4, 6, -10),nrow = 3,ncol = 3)
## myMatTest <- makeCacheMatrix(myMat)
## cacheSolve(myMatTest)
##------------------------------------------------------------

##------------------------------------------------------------
## makeCacheMatrix
## This function will create a cache of the matrix passed into
## the function.  It also contains the code to change the
## cached matrix, calculate the inverse and store the inverse
## of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inv){
                inverse <<- inv
        }
        getinverse <- function(){
                inverse
        }
        list( set = set
             ,get = get
             ,setinverse = setinverse
             ,getinverse = getinverse
            )
}

##------------------------------------------------------------
## cacheSolve
## This function will return an inverse matrix of the passed
## matrix if a cached version does not exist.  If an inverse
## exists in the cache, this cached version will be returned
## instead of recalculating the inverse and a message noting
## the cache was used will be printed

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                print("Getting Cached Inverted Matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
