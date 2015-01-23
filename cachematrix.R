## makeCacheMatrix creates an object which is similar to a matrix x
# but adds 4 properties :
# inv : the value of inverse of x
# set : "manually" sets the value of x
# get : gets the value of x
# getInv : gets the inverse of x
# setInv : sets the computation of the inverse to rely on the function "solve"

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve computes the inverse of a matrix x
# if the inverse has already been computed : cacheSolve returns
# the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
