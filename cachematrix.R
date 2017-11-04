## 'makeCacheMatrix' takes a square matrix as input, sets inverse matrix to NULL
## and saves this null inverse and input matrix into cache. Further it defines
## 'get' function for saved matrix and 'setinverse' and 'getinverse' to set and
## get inverse matrix respectively

## 'cacheSolve' takes the cache saved matrix from function 'makeCacheMatrix'
## and looks if the inverse is computed for this matrix or not. If it is, then
## it reads it from cached data and returns the inverse, and if not, then it
## computes the inverse, saves it in cache and then returns the computed value



## Input and output setting function

makeCacheMatrix <- function(x = matrix()) {
	inverse = NULL
	set <- function(y){
		matrix <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Output returning function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse = x$getinverse()
	if(!is.null(inverse)){
		message("getting the cached data")
		return(inverse)
	}
	data = x$get()
	inverse = solve(data)
	x$setinverse(inverse)
	inverse
}
