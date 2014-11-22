makeCacheMatrix <- function(x = matrix()) {# v <- makeCacheMatrix assign a matrix with inverse cache support to v
        inverse <- NULL
        set <- function(y) {
                x <<- y # set x to Matrix chosen above 
                inverse <<- NULL # there is a place holder matrix yet
        }
        get <- function() x # v$get() produces the matrix 
        setinverse <- function(solve) inverse  <<- solve 
        getinverse <- function() inverse # v$getinverse() retrieves the inverse of matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {# cacheSolve(v) computes inverse of v
        inverse <- x$getinverse() # assigns the inverse of x to inverse
        if(!is.null(inverse)) { 
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...) # Solves for the inverse(x) and stores it in varaible inverse
        x$setinverse(inverse) 
        inverse
}