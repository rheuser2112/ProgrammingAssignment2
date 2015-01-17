## The following two functions can be used to create a matrix object that
## caches the inverse: makeCacheMatrix
## Fn cacheSolve uses the special object and if the matrix was already calcuated
## it returns the cached result, otherwise it "solves" the inverse matrix and set it 
## to the special matrix object 

## creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(invmat) m <<- invmat
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## The following code can be used to generate a square invertible matrix 
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8); h8

## ma=makeCacheMatrix(h8)
## cacheSolve(ma)          ## here the ma is calculated the first time and solve is called
## cacheSolve(ma)          ## 2nd time ma isn't NULL and therefore it is getting the value from cache
