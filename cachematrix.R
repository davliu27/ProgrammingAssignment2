## Below are two functions that are used to create a special object that
## stores a matrix and caches its inverse matrix.

## makeCacheMatrix creates a special "vector", which is a list containing a 
#function to:
#1. set the matrix
#2. get the matrix
#3. set the inverse matrix
#4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  MatInv <- NULL
  set <-function(y){
    x <<- y
    MatInv <<- NULL
    
  }
  
  get <- function() x
  setMatInv <-function(MatInv) MatInv <<- MatInv
  getMatInv <-function() MatInv
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
}


## cacheSolve calculates the inverse matrix of the inputted matrix created
#with the makeCacheMatrix function.  However, it first checks to see if the
#inverse matrix has already been calculated.  If so, it 'get's the inverse
#matrix from the cache and skips the computation.  Otherwise, it calculates 
#the mean of the data and sets the value of the mean in the cache via the 
#setMatInv' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  MatInv <- x$getMatInv()
  if(!is.null(MatInv)) {
    message("getting cached data")
    return(MatInv)
  }
  
  data <- x$get()
  MatInv <- solve(data,...)
  x$setMatInv(MatInv)
  MatInv
}
