## R Programming: Programming Assignment 2
## simpsoch
## ---
## Fuction 1 of 2: makeCacheMatrix
## Creates a matrix that will cache its own inverse.
## Saves the inverse to variable 'x'.
## Saves the inverse of 'x' to variable 'v' for scope.
## Solution Credit: user3643053 and agstudy. See URL: 
## http://stackoverflow.com/questions/23796316/returning-the-inverse-matrix-from-a-cached-object-in-r
## Does the assignment require named matrix object, or is a mere abstraction of the matrix object acceptable?

makeCacheMatrix <- function(x = matrix()) { 	
	v <- NULL 
	set <- function (y)	{
		x <<- y
		v <<- NULL
	}
	get <- function() {
		x
	}
	setSolve <- function(solve)	{
		v <<- solve
	}
	getSolve <- function() {
		v
	}
	list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## Function 2 of 2: cacheSolve function
## Computes and returns inverse of aforementioned matrix 'x'
## Solution Credit: theofpa. See URL:
## https://github.com/theofpa/datasciencecoursera/blob/master/cacheSolve.R

cacheSolve <- function(x, ...) {
	v <- x$getSolve()  					## should be x$getMatrix
	if(!is.null(v))	{
		message ("getting cached data")
		return(v)
	}
	matrix <- x$get()					## replace 'matrix' with object name
	v <- solve(matrix, ...)
	x$setSolve(v)
	v	
}
