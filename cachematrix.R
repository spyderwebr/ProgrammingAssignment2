## make cache matrix function
## set matrix data, initial cache value

makeCacheMatrix <- function(x = matrix()) {
	invMat <- matrix(data = NA)  		## initialize invMat to NA
	set <- function(y) {
		x <<- y
		invMat <<- matrix(data = NA)  ## cache NA value for invMat
	}
	get <- function() x
	setinvMat <- function(solve) invMat <<- solve
	getinvMat <- function() invMat
	list (set = set, get = get, setinvMat = setinvMat,
		getinvMat = getinvMat)
}

## inverse matrix cache function
## report solve(): calculate if not cached, else report cached value

cacheSolve <- function(x, ...){
	invMat <- x$getinvMat()  	## retrieve cached solve() value from makeCacheMatrix()
	if(!is.na(invMat)) {	 	## determine if cache contains solved value
		message("retrieved cached solve() results")
		return(invMat)		## return cached value if available
	}
	data <- x$get()
	invMat <- solve(data, ...)	## determine solve() if NA cached
	x$setinvMat(invMat)		## set determined solve() in cache
	invMat				## return solve()
}

