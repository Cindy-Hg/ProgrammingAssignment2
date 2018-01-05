## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function follows 4 steps 
## 1.Set the value of the vector
## 2.Get the value of the vector
## 3.Set the value of the inverse
## 4.Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set  <-  function(y){
		x <<- y
		inv <<- NULL 
	}
	get <- function() x
	setinverse <- function(inverse) inv <- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The cacheSolve function uses a "IF" expression 
## 1. IF the inverse of the matrix has been calculated and is stored in the cache, then it returns the matrix value stored in cache
## 2. ELSE it calculates the inverse of the matrix and stores it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
