## Descript of the functions
#The MakeCacheMatrix function, creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the Inverse of matrixve
#get the inverse of matrix

##Assumptions:
#the input matrix to makeCacheMatrix functions is always invertible

makeCacheMatrix <- function(x = matrix()) {
  #inverse of a matrix
  im <- NULL
  
  set <- function(splMatrix) {
    x <<- splMatrix
    im <<- NULL
  }
  
  get <- function () x
  setInvMat <- function (solve) {
    #print(invMatrix)
    im<<- solve
  }
  getInvMat <- function() im
  list (set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}


#The function calculates the Inverse of Matrix of the special "matrix" created with the makeCacheMatrix function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data (matrix) and sets the value of the inverse in the cache via the setInvMat function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  print (x$getInvMat)
  im <- x$getInvMat()
  if (!is.null(im)) {
    message("Getting cache data")
    return (im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInvMat(im)
  im
}