## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix returns a list with the orginal matrix and its inverse
## set method within is used to initialize the cached data
## get method within is used to retrive the cached data

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set<-function(y){
      x <<- y
      i <<- NULL
    }

    get<-function()x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## Write a short comment describing this function
## cacheSolve utilizises the list created by makeCache matrix
## if the inverse set there is inverse then new inverse will 
## be calculated by using Solve method over the matrix
## the new inverse will be stored within the list again using the setinverse
## if its not null then the value will be directly returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data");
        return(m)
    }
    data <- x$get()
    m <- Solve(data)
    x$setinverse(m)
}
