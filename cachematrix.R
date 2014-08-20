## The makeCacheMatrix() and cacheSolve() functions work together to compute
## and cache the inverse of a matrix either initially passed in as an argument
## to makeCacheMatrix() or set later by a call to the set() function as defined
## in the local scope of makeCacheMatrix().
##
## The cacheSolve() function computesthe inverse of the matrix stored in the
## makeCacheMatrix closure.


## Function Name:  makeCacheMatrix
## INPUT: mx - a matrix object initialized to an empty matrix by default if 
## argument is missing
##
## OUTPUT: a list object holding references to 4 functions defined in the closure
##
## Description: this function caches the inverse of a matrix and defines 4 
## functions in its closure or local scope to 
## 1) set the local variable acting as the cache to NULL when a new matrix object is set
## 2) get the matrix object that was passed into the function as argument
## 3) set the inverse of the matrix passed in as argument to the set() function
## 4) get the cached inverse of the matrix that was passed in by calling makeCacheMatrix() or
##    or set()

makeCacheMatrix <- function(mx = matrix()) {
    inv_matrix  <- NULL
    
    set <- function(new_matrix) {
        mx <<- new_matrix
        inv_matrix <<- NULL
    }
    
    get <- function() mx
    
    setInvMatrix <- function(cached_invmx) {
        inv_matrix <<- cached_invmx
    }
    
    getInvMatrix <- function() inv_matrix
    
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Function Name:  cacheSolve
## INPUT:
##
## OUTPUT:
##
## Description:
##

cacheSolve <- function(mx, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmx_cached <- mx$getInvMatrix()
    
    if (!is.null(invmx_cached)) {
        message('getting cached data')
        return(invmx_cached)
    }
    
    mx_cached <- mx$get()
    invmx_cached <- solve(mx_cached)
    mx$setInvMatrix(invmx_cached)
    invmx_cached
}

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) {
        m <<- mean
    }
    getmean <- function() m

    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

show_env <- function(){
    list(ran.in = environment(),
         parent = parent.env(environment()),
         objects = ls.str(environment()))
}