## Sets a special matrix to atomic variable x and creates a few more functions (see list)

makeCacheMatrix <- function(x = matrix()) {# v <- makeCacheMatrix assign a matrix with inverse cache support to v
        inv <- NULL
        set <- function(y){
                x <<- y # set x to Matrix chosen above
                inv <<- NULL
        }
        get <- function() x # Retrieves matrix x; v$get() produces the matrix 
        setinv <- function(solve) inv <<- solve # Assigns inv to what you say
        getinv <- function() inv # Retrieves the inv matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Caches, computes and returns the inverse (inv) of matrix x from makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #Assigns inv to be the inverse of matrix x
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # Sets datvvariable to matrix x
        inv <- solve(data) # Solves for the inverse(x) and stores it in varaible inv
        x$setinv(inv) # Assigns to inv the inverse(x)
        inv # Returns the inverse(x)
}
