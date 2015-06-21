## Develop a pair of functions:
##  1.  makeCacheMatrix - which creates a special "matrix"
        object that can cache its inverse
##  2.  cacheSolve - which creates or retrieves from cache
        the inverse of the matrix returned by makeCacheMatrix 

##  Step 1 - Define the function "makecacheMatrix". 
##           The function argument "x" is the matrix
##           whose inverse is to be computed.

      makeCacheMatrix <- function(x = matrix()) {

##  Set the value of the Inverse of matrix "x" to NULL
##  If a new matrix is provided, reset the value 
##    of "x" in above function, and the value of 
##    Inverse in cache to NULL.

      Inverse <- NULL
      set <- function (y) {
          x <<- y 
          Inverse <<- NULL
   }

##  Use the "get" function to get the matrix "x" 
##    whose inverse is to be found from above function.
##  The "solve" function is used to find the inverse 
##    of the matrix, and assign to "Inverse".
##  The function "setinverse" stores the value of "Inverse"
##    into the main function. 
##  The "getinverse' function is used to retrieve the value
##    of "Inverse" from the main function. 
##  "list" is defined to be the output of the four functions:
##     set, get, setinverse and getinverse
      
      get <- function()  x
      setinverse <- function(solve)  Inverse <<-  solve
      getinverse <- function()  Inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
 
    }

##  The function "cacheSolve" retrives the value of inverse 
##    stored in the main function. If said value is not NULL, 
##    it prints the message "getting cached inverse" and
##     returns the value of the inverse. 
##  If the cached value for inverse is NULL, the matrix"x"
##     is retrieved, its inverse computed and output, 
##     and also stored in cache.

      cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

      Inverse <- x$getinverse()
      if(!is.null(Inverse))  {

           message("getting cached inverse")
           return(Inverse)
   }
      
      Matrix <- x$get()
      Inverse <- solve(Matrix,...)
      x$setinverse(Inverse)
      Inverse

   }

##  Test of above functions to find inverse of 4 x 4 matrix
##  Matrix <-  matrix(c(0,1,1,3,4,1,-2,0,0,5,0,0,-3,2,6,1),4,4)
##  M <-  makeCacheMatrix(Matrix)
##  cacheSolve(M)
