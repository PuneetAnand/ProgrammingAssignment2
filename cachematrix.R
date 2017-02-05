#The two functions are used to cache the inverse of a matrix.
#First function is to initate a matrix and store Inverse value in parent environment(<<-)
#Second function to check whether inverse value is cached else calculate if it is NULL 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <-  function  (y=matrix()) {
        inv <<- NULL
        x <<-y
    }
    
    get <- function () x
    getInv <-  function()  inv
    setInv <- function(inverse) inv <<- inverse
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}   
    
     


# The function returns the inverse of the matrix.This function can only accept input argument 
#of makeCacheMatrix().
# It first checks ifthe inverse has already becaen computed. If so, it gets the result.
#  If not, it computes the inverse, sets the value in the cache via
# setInv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
      Inv <- x$getInv()
      if (!is.null(Inv)) {
          message("getting cached data")
          return(Inv)
          
      }
      
      data <- (x$get())
      Inv <- solve(data)
      x$setInv(Inv)
      Inv
    }
