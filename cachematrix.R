# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store and return a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# 1. setMat      set the value of a matrix
# 2. getMat      get the value of a matrix
# 3. cacheInv    Cache the value (inverse of the matrix)
# 4. getInv      get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
  
  #Cache variable intialized
  c <- NULL
  
  # store a matrix
  setMat <- function(matrixData) {
    x <<- matrixData
    c <<- NULL   # Everytime matrix gets updated, we should clear the cache.  
  }
  
  # returns the stored matrix
  getMat <- function() {
    x
  }
  
  # cache the given argument(Inverse of the matrix will get cached) 
  cacheInv <- function(store) {
    c <<- store
  }
  
  # get the cached value
  getInv <- function() {
    c
  }
  
  list(setMat = setMat, getMat = getMat, cacheInv = cacheInv, getInv = getInv)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(y, ...) {
  
  # get the cached value by calling the below fumction
  inv <- y$getInv()
  
  # if a cached value exists return it
  if(!is.null(inv)) {
    message("Cached data is as follows")
    return(inv)
  }
  
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMat()
  inv <- solve(data) #solve() --> This function calculates the inverse of a square matrix
  y$cacheInv(inv)
  
  # return the inverse
  inv
}