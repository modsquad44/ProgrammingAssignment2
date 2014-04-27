# Two functions, the first calculates and caches the inverse of a matrix, the second takes a matrix and calculates the inverse if not already cached, first it performes a check to make sure original matrix object and inverse matrix object have not been modified since makeCacheMatrix() was last called

rm(list=ls())

# The following function creates a special "matrix" object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# calc and cache matrix inv
	inv.x <- solve(x)
	inv.x.cache <<- inv.x
	
	# need to create a simple check for the cacheSolve() function to determine if...
	# the input matrix (x) and/or (inv.x.cache) have been modified before calling cacheSolve() 
	# (meaning a new inverse needs to be calculated).
	
	# the values for this check will be created here and then pulled for a match in cacheSolve()
	x.col.check <- x[,as.integer(ncol(x)/2)] # creates vector of center column in matrix(x)
	x.row.check <- x[as.integer(nrow(x)/2),] # creates vector of center row in matrix(x)
	
	inv.x.col.check <- inv.x.cache[,as.integer(ncol(inv.x.cache)/2)] # creates vector of center column in inv.x.cache
	inv.x.row.check <- inv.x.cache[as.integer(nrow(inv.x.cache)/2),] # creates vector of center row in inv.x.cache
	
	# export a list with check values
	checklist <<- list(x.col.check, x.row.check, inv.x.col.check, inv.x.row.check)
	
	message("Inverse Matrix Calculated - resulting matrix object exists in --> inv.x.cache")
}


# The following function computes the inverse of a matrix, if the inverse matrix has already been cached and is unchanged, then it does no calcualtion and retrieves the inverse matrix from cache.

cacheSolve <- function(x, ...) {
     
	if(!is.null(inv.x.cache)) { # if cache exists do the following checks...
	
		# calculate new check values and export to a list to compare with existing checklist
		x.col.check.new <- x[,as.integer(ncol(x)/2)] # creates vector of center column in matrix(x)
		x.row.check.new <- x[as.integer(nrow(x)/2),] # creates vector of center row in matrix(x)
	
		inv.x.col.check.new  <- inv.x.cache[,as.integer(ncol(inv.x.cache)/2)] # creates vector of center column in inv.x.cache

		inv.x.row.check.new  <- inv.x.cache[as.integer(nrow(inv.x.cache)/2),] # creates vector of center row in inv.x.cache
		
		checklist.new <- list(x.col.check.new, x.row.check.new, 
						inv.x.col.check.new, inv.x.row.check.new)
		checkval <- sum(match(checklist,checklist.new), na.rm=TRUE) # sums a vector of matching elements (which should = 1:4, so a positive match = 10)
		
		if ( # if checkval = 10 (positive match), return the cached inv matrix
			checkval == 10) {
			message("Inverse Matrix Retieved (input matrix and existing cache passed match test) - resulting matrix object exists in --> inv.x.cache")
			return(inv.x.cache)
		} else { # solve/return inv matrix
			inv.x <- solve(x)
			inv.x.cache <<- inv.x
			message("Inverse Matrix Calculated (input matrix or existing cache did not pass match test) - resulting matrix object exists in --> inv.x.cache")
			return(inv.x.cache)
        		} 
     	} else { # solve/return inv matrix
        	inv.x <- solve(x)
		inv.x.cache <<- inv.x
		message("Inverse Matrix Calculated (no cache exists) - resulting matrix object exists in --> inv.x.cache")
		return(inv.x.cache)
       	}
}


# tests

# set.seed(45)
# vals = sample(1:50, 2500, replace=TRUE)
# x = matrix(vals, 50, 50)
# set.seed(50)
# vals = sample(1:50, 2500, replace=TRUE)
# x2 = matrix(vals, 50, 50)
# makeCacheMatrix(x)
# cacheSolve(x2)

