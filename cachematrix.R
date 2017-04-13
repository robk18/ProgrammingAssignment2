## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function sets and gets x and inverse x
#run by x1<-makeCacheMatrix(m1) then cacheSolve(x1)
makeCacheMatrix <- function(x = matrix()) {
   invmatrix <- NULL  #invmatrix will store the inverse matrix, initialize it to null
   #Set the value of the matrix and inverse matrix to null
   set <- function(y) {
   x <<- y
   invmatrix <<- NULL
  }
  get <- function() x   #get x matrix
  setinv <- function(invx) invmatrix <<- invx   #set the inverse matrix
  getinv <- function() invmatrix   #get the value of the inverse matrix
  #create the list of functions as described above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#get the cached value of matrix  and if matrix not changed or solve if doesn't exist
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinv() #retrieve the value of inverse of x
  if(!is.null(invmatrix)) { # if the value of inverse exists
    message("getting cached data")  #report value exists
    return(invmatrix) #output existing value
  }
  data <- x$get()  #if value for inverse is null then get x
  invmatrix <- solve(data, ...) #calculate the inverse matrix of x
  x$setinv(invmatrix) #set the inverse matrix in the cache
  invmatrix #output the inverse matrix
}
