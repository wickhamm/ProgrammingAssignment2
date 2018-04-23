#############################################################################
#           Programming Assignment 2 - Maeve Wickham
#############################################################################

# 1) Make the cacheMatrix function: this creates a list with 
#                                   a function to set a matrix's value, 
#                                   get the matrix's value,
#                                   set the inverse value of the matrix, 
#                                   and get the inverse value of the matrix. 


makeCacheMatrix <- function(x, ...){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse #find the inverse of the matrix
  getinv <- function() i
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
} 

# 2) Make the cacheSolve function: this will return the matrix's inverse. 
#                                 First, it checks whether the inverse has 
#                                 already been identified, in which case it
#                                 skips computation by accessing the stored 
#                                 inverse. If not, this function computes
#                                 the inverse and sets the value in the cache.

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
  }
