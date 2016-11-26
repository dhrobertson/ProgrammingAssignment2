## cachematrix.R
##
## Create a cache matrix object that can compute and
## store the matrix inverse so that successive calls
## does not need to compute the inverse again if it 
## already exists in the cache
##
## matrix inverse is computed with the R "solve" function
##
## Sample matrix "c"
##      c=rbind(c(1, -1/4), c(-1/4, 1))
##
## Example using example matrix c
##      cMatrix <- makeCacheMatrix(c)
##      z <- cacheSolve(cMatrix)
##
## Results of Example
##
##      getting cached
##      > z
##               [,1]      [,2]
##      [1,] 1.0666667 0.2666667
##      [2,] 0.2666667 1.0666667
##
## Additional functions
##      cacheMatrix$set(x) # reset the cached matrix to x
##      cacheMatrix$get()  # return cached matix
## 

## This is the function that caches the matrix and its inverse
## It defines the various available functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This is the function to actually retrieve the inverse
## from cache, if available, or else generate the inverse 
## and store it in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        invFn <- solve(data, ...)
        x$setInverse(invFn)
        inv
}
