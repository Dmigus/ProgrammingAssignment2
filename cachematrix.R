# function return special matrix represented by set of four function
makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y){
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setSol <- function(solution) sol <<- solution
    getSol <- function() sol
    list(set = set, get = get, setSol = setSol, getSol = getSol)
}

# return cached value of special matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    sol <- x$getSol()
    if (!is.null(sol)){
        message("getting cached data")
        return(sol)
    }
    sol <- solve(x$get(), ...)
    x$setSol(sol)
    sol
}

#test
# specMatr <- makeCacheMatrix()
# specMatr$set(matrix(sample(9), ncol = 3))
# print(cacheSolve(specMatr))
# print(cacheSolve(specMatr))
