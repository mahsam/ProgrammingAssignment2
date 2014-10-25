# the purpose of writing these two function is to cache the inverse of a matrix
# computing the inverse of a matrix could be 
# an expensive operation from time and resources perspective
# so caching helps us to avoid redundant computations and improve
# the efficiency.


# this function get a matrix as input and 
# returns a list of functions (get, set, getinverse, setinverse)
makeCacheMatrix <- function(x = matrix()) {
    # local variable s defined and null is assigned as the initial value
    s <- NULL
    
    # get function returns matrix x
    get <- function() x
    
    # set function sets matrix x to a new matrix
    set <- function(y) {
        # y as new matrix will be assigned to 
        # variable x which is declared outside of this function
        x <<- y
        
        # reset variable s which is declared outside of this function
        # we actually reset the inverse value for the new matrix
        s <<- NULL
    }
    
    # returns s as the inverse of x
    getinverse <- function() s
    
    # sets the value solve is assigned to
    # to variable s as the inverse of matrix x
    # we use superassignment operator since s is declared 
    # outside of this function
    setinverse <- function(solve) s <<- solve 
    
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}

# cacheSolve function returns the cached inverse of matrix if it exists.
# otherwise it computes the inverse and caches the value
# in order to avoid redundant computations.
cacheSolve <- function(x, ...) {
    # get the inverse of matrix x
    s <- x$getinverse()
    
    # if the inverse of matrix x is already calculated and it is not null,
    # we return the cached inverse matrix
    if (!is.null(s)) {
        message("Returning cached matrix!")
        return(s)
    }
    
    # since the inverse does not exist, we need to calculate and return it
    # x$get returns the actual matrix
    mat <- x$get()
    # solve computes the inverse of mat
    s <- solve(mat, ...)
    # variable s is the inverse of mat and setinverse caches the value
    # so next time we want to access it, we dont' have to compute
    # the inverse again and we use the cached value
    x$setinverse(s)
    s
}