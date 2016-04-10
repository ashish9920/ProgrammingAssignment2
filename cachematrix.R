## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL
        
         set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) {
  
  calculatedInverse <- x$getInverse() 
  
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    return(calculatedInverse)
  }
  matrixToSolve <- x$get()  
  
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("value of inverse:") 
  x$setInverse(calculatedInverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
