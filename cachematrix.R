
makeCacheMatrix <- function(x = matrix()) {
#		these lines define variables to be called later on
    inverse <- NULL
   	set <- function(y) {
        x <<- y
        inverse <<- NULL
}

#		this assigns the 'get' to function x  
    get <- function() x
    
# 		this sets inverse and sets the inv to inverse
    setinv <- function(inv)
	inverse <<- inv

#		this sets function() inv to get inv
    getinv <- function() inverse
    
#		this returns the setinv and get inv variables so they can be called by cachesolve
#		does so in way identical to standard defined in makeVector.r provided 
    list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)    
}


#		this returns the inverse of matrix x
cacheSolve <- function(x, ...) {

#		this assigns the get inv with respect to x to inverse
    inverse <- x$getinv()
    
#		in this inverse is cached and the value computed is then returned
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
}
    
# 		this calculates inverse and assigns the value to the variable
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse    
}


# 		This code bellow is used to demonstrate cacheSolve() works
 
# 	matrix <- matrix(sample(1:100, 8), 3, 3)
# 	testcase <- makeCacheMatrix(matrix)
# 	cacheSolve(testcase)
