## The two functions below create a special object that stores a matrix, 
##and caches it's inverse

##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse, using a list of 4 functions that set 
##the value of the matrix, get the value of the matrix, set the value 
##of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. First, it checks to see if the inverse 
##has already been calculated (and the matrix has not changed), and if it has, 
##it will return the inverse from the cache. If the inverse has not been 
##calculated, the function will calculate and set the inverse via the set_inv function.

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}
