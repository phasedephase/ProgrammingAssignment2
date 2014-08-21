## The cacheSolve() and makeCacheMatrix() functions work together to compute
## and cache the inverse of a matrix either initially passed in as an argument
## to makeCacheMatrix() or set later by a call to the set() function as defined
## in the local scope of makeCacheMatrix().
##
## The cacheSolve() function computes the inverse of the matrix stored in the
## makeCacheMatrix() closure.


## Function Name:  makeCacheMatrix
## INPUT: mx - a matrix object initialized to an empty matrix by default if 
## argument is missing
##
## OUTPUT: a list object holding references to 4 functions defined in the closure
##
## Description: Cache the inverse of a matrix and define 4 functions in the
## closure or local scope in order to
## 1) set(): set a new matrix and set the local variable acting as the cache to NULL 
## 2) get(): get the matrix object that was passed into the function as argument
## 3) setInvMatrix(): set the inverse of the matrix passed in as argument to 
##                    either makeCacheMatrix() or the set() function
## 4) getInvMatrix(): get the cached matrix inverse

makeCacheMatrix <- function(mx_init = matrix()) {
    invmx_cached  <- NULL
    
    set <- function(new_mx) {
        mx_init <<- new_mx
        invmx_cached <<- NULL
    }
    
    get <- function() mx_init
    
    setInvMatrix <- function(invmx) {
        invmx_cached <<- invmx
    }
    
    getInvMatrix <- function() invmx_cached
    
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Function Name:  cacheSolve
## INPUT: mx_init - a list object returned by makeCacheMatrix() holding 4 functions
##        to manipulate a matrix and its inverse
##        ... - additional args are not used at the moment
##
## OUTPUT: invmx_cached - the inverse of the matrix initially passed into 
##         makeCacheMatrix()
##
## Description: Retrieve from the list object passed as arg and made by makeCacheMatrix()
##              the inverse of the matrix used when first calling makeCacheMatrix().
##              If cacheSolve() has already been called once, then return the 
##              matrix inverse that is cached in the closure of the makeCacheMatrix() 
##              function. If cacheSolve() is called for the first time, then 
##              there is no inverse matrix cached yet, and we retrieve the initial
##              matrix passed as an arg to makeCacheMatrix() or set via the  
##              set() function defined in the closure of makeCacheMatrix(). We
##              then call solve() on this matrix to get its inverse and cache it
##              with a call to setInvMatrix() before returning it.

cacheSolve <- function(mx_init, ...) {
    invmx_cached <- mx_init$getInvMatrix()
    
    if (!is.null(invmx_cached)) {
        message('getting cached data')
        return(invmx_cached)
    }
    
    mx <- mx_init$get()
    invmx_cached <- solve(mx)
    mx_init$setInvMatrix(invmx_cached)
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