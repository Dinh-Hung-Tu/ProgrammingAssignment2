## Put comments here that give an overall description of what your functions do:

## Write a short comment describing this function:
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv 
        getInv <- function() i 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## Write a short comment describing this function

## The following function calculates the inverse (abbr inv) of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache via the function getInv() and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv() function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i<-x$getInv()
		if(!is.null(i)){
			message("getting cached data")
			return(i)
		}
		data<-x$get()
		i<-solve(data)
		x$setInv(i)
}

## Test case:
myMatrix<-c(1,2,3,4);
dim(myMatrix)<-c(2,2);
myFunc<-makeCacheMatrix(myMatrix)
cacheSolve(myFunc)
myFunc$getInv()
