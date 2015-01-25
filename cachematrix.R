makeCacheMatrix <- function(x = matrix()) {
    
    invm <- NULL
    
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    
    get <- function() x
    
    setinvm <- function(inv) invm <<- inv
    
    getinvm <- function() invm
    
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}

cacheSolve <- function(x, ...) {
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}