## cachematrix is composed of 2 functions: makeCacheMatrix & cacheSolve 
## 
## How to use:
## Create a square matrix: x <- matrix(rnorm(25), nrow=5)
## Create the "special" matrix: spx <- makeCacheMatrix(x)
## Calculate inverse matrix: cacheSolve(spx)
## Re-calculate inverse matrix to show cached value is returned: cacheSolve(spx)

## makeCacheMatrix creates a special matrix
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      ## sets invm equal to NULL
      invm <- NULL

      ## subfunction to set matrix
      setm <- function(y){
            x <<- y
            invm <<- NULL
      }
      ## subfunction to get matrix      
      getm <- function()x

      ## setting inversematrix
      setinvm <- function(inverse)invm <<- inverse

      ## getting inversematrix
      getinvm <- function()invm

      ## Returns the matrix with defined functions
      list (setm= setm, getm= getm,
            setinvm= setinvm, getinvm= getinvm)
}

## cacheSolve calculates the inverse of the matrix.
## if the inverse has already been calculated, it returns the cached inverse

cacheSolve <- function(x, ...) {
      ## Retrieves most recent value of the inverse
      invm <- x$getinvm()
      
      ## checks if invm has already been calculated
      ## (and matrix not changed)
      ## and returns its value from cache if value of invm is not NULL
      if (!is.null(invm)){
            message("getting cached value")
            return(invm)
      }
      ##Calculates inverse of matrix x, if invm is NULL
      data <- x$getm()
      invm <- solve(data,...)
      
      ##Caches the inverse
      x$setinvm(invm)
      
      ##returns inverse matrix
      invm
}
