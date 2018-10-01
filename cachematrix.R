#This function is able to cache the invers of a matrix as special object. 
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
  inversa <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) inversa <<- solveMatrix
  getInv <- function() inversa
  list(set = set, get = get,setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

#This function calculates the inverse of the matrix returned by makeCacheMatrix function defined before. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getInv()
  if(!is.null(inversa)){
    message("Getting chache...hold on")
    return(inversa)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inversa)
  inversa 
}
