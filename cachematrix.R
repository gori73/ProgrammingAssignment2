## makeCacheMatrix function outputs a list of four functions that store the original matrix (get) as well as the 
## cached inverted matrix that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) {
    x <<- y
    inv<<- NULL
}

  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then  cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()                #try to get cashed inverse matrix
    if(!is.null(inv)) {                  #if not empty 
      message("getting cached data")     #print out message that there is already cashed data
      return(inv)   # output the inverse cashed matrix and stop
    }
    #if there is no cashed data:
    #get the original matrix from the corresponding element (get) of the list created by makeCacheMatrix
    data <- x$get()   # get 
    inv <- solve(data, ...)  #solve is the function that inverses the matrix
    x$setinverse(inv)
    inv
    
  }
}


## to use these two functions need first create the original matrix                      
## then  assign the result of makeCacheMatrix  (list of four functions)  to a variable
##  and, finally, run cacheSolve with this variable as an argument :   

mat<-matrix (1:4,2)
a<-makeCacheMatrix(mat)
cacheSolve(a)

#if cacheSolve(a) is run again, it will print the message "getting cached data"
