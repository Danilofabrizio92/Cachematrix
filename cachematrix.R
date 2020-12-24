## Firt of all, I'm caching the Inverse of a matrix as follows:
## Let's talks about The Matrix inversion, since is usually a costly computation approach, able to provide  
## a useful know-how to caching the inverse of a matrix rather than applied it steadily.
## So that, as reported in the following scripts, a pair of functions are employed to build an object, which 
## keep in a matrix and caches its inverse. 
## Regarding the role of this function, it creates a 'matrix' object that might cache its inverse called "getsolve".
##makeCacheMatrix function is below:

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
    
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##This functions allows the inverse of the "matrix" created by
##makeCacheMatrix seen above. Nevertheless, If the inverse has already been evaluated (So that,
## the matrix has not been replaced), then it should recover the inverse from the cache.
cacheSolve <- function(x, ...) { 
  ## Return a matrix which is the inverse of the "x" called "getsolve".
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  mat <- x$get()
  s <- solve(mat, ...)
  x$setsolve(s)
  s
}
source("R_test_pollution_coursera_courseRStudio.R")
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()

     [,1] [,2]
[1,]    1    3
[2,]    2    4

> my_matrix$getsolve()
NULL

> cacheSolve(my_matrix)
##The inverse has been carried out by the cacheSolve function, and the results are reported below
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> my_matrix$getsolve()
	[,1] [,2]
[1,]      -2  1.5
[2,]       1 -0.5

> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
> my_matrix$get()
      [,1] [,2]
[1,]    2    1
[2,]    2    4

> my_matrix$getsolve()
NULL
> cacheSolve(my_matrix)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

> cacheSolve(my_matrix)
getting cached data
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

> my_matrix$getsolve()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

