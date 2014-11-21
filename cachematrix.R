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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
