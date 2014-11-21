## Sets a special matrix to atomic variable x and creates a few more functions (see list)
## This takes advantage of the lexical scoping rules R follows

makeCacheMatrix <- function(x = matrix()) {# v <- makeCacheMatrix assign a matrix with inverse cache support to v
        inv <- NULL
        set <- function(y){
                x <<- y # set x to Matrix chosen above
                inv <<- NULL
        }
        get <- function() x # Retrieves matrix via v$get()
        setinv <- function(solve) inv <<- solve # Assigns inv to what you solved
        getinv <- function() inv # Retrieves the inv matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Caches, computes and returns the inverse (inv) of matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv() # Assigns inv to be the inverse of matrix
        if(!is.null(inv)){ # if the inverse has already been calculated just return it without calculating again
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # Sets data variable to matrix x
        inv <- solve(data) # Solves for the inverse and stores it in varaible inv
        x$setinv(inv) # Assigns to inv the inverse matrix
        inv # Returns the inverse matrix
}
