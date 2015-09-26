##Caching sd of a vector
## caching the standard deviation of a vector
## the first function s sets the standard deviation to null as a placeholder 
## set sets the vector x to a new vector y and resets the standard deviation s to NULL
#get returns the vector x value
#setsd sets the standard deviation s to sd
#getsd returns the standard deviation of s
#list returns the special vector of the standard deviation functions which were defined
#caching sd of a vecto
s<- null
set <- function(y) {x <<- y; s <<- NULL}
get <- function() x
setsd <- function(sd) s <<- sd
getsd <- function() s
list(set = set, get = get,setsd = setsd,getsd = getsd)

#cache the inverse of a matrix function project 2
#does this by setting and getting the values of the matrix and the inverse.







makeCacheMatrix <- function(x = matrix()) {
  
  u <- NULL
  set <- function(y) {
    x <<- y
    u <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}
#This function below computes inverse of the matrix returned.
cachesolve <- function(x, ...) { 
  i<- x$getinv()
  if(!is.null(u)) {
    message("getting cached data")
    return(u)
  }
  data <- x$get()
  u<- solve(data, ...)
  x$setinv(u)
  u
  
}
