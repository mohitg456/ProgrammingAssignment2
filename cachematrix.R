## This file contains the code for weeks 3 programming assignment 2 to create a special matrix object that exposes 
## functions to set its values, retrieve its values, cache its inverse and retrieve the cached inverse
## Input - (optional) values to store in the matrix at the time a new instance is created. Type matrix
## Output - Returns a list of 4 functions that can be used to operate on the object.

makeCacheMatrix <- function(m = matrix()) {
	## This function can be used to create an object that can store a matrix and its inverse when the 
	## inverse is computed for the first time
	
	## The following initialization ensures that the stored inverse is nulled out
	## in case someone changes the matrix without going through the set_values method
	## as done below for test case 4.  If the following line is commented, test cases 4 and 5 will not work
	inv_m <- matrix(nrow=0, ncol=0)
	
	set_values <- function(values_to_set) {
		m <<- values_to_set
		inv_m <<- matrix(nrow=0, ncol=0)
	}
	get_matrix <- function() {
		return(m)
	}
	cache_inverse <- function(inversed_matrix) {
		inv_m <<- inversed_matrix
	}
	get_inverse <- function() {
		return(inv_m)
	}
	return( list(set = set_values, get = get_matrix,
				 cache_inv = cache_inverse,  get_inv = get_inverse)
	)
}


cachesolve <- function(x, ...) {
	## This is the function that can be called to return the inverse of the matrix stored  
	## in a makeCacheMatrix object. Returns cached inverse if available and matrix is unchanged.
	
	cached_inv <- x$get_inv()
	if(length(cached_inv) != 0) {
		message(">>>found cached inverse")
		return(cached_inv)
	} else {
		message(">>>not cached... calculating and caching")
		new_inv <- solve(x$get(), ...)
		x$cache_inv(new_inv)
		return(new_inv)
	}
}

##########################################
## TEST SCRIPT
##########################################

## generate a 4x4 square matrix to test
testData <-matrix(sample(9), 3, 3)		

## Make a new matrix using our cacheable function
testM <- makeCacheMatrix()

## case 1. call the set function to load test values
testM$set(testData) 						

## case 2. call cachesolve to verify it calculates the inverse 
cachesolve(testM)

## case 3. call cachesolve again to verify it gets the inverse from cache
cachesolve(testM)

##Change the values of the matrix
testM <- makeCacheMatrix(matrix(sample(9),3, 3))

## case  4. call cachesolve to verify it recalculates inverse after data has been changed
cachesolve(testM)

## case 5. call one last time to see it fetches from cache now
cachesolve(testM)