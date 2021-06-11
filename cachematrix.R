## These functions are designed to make the process of computing the inverse of
## a matrix more efficient by first computing the inverse and subsequently caching
## the result.

## Takes a matrix as input and produce a special matrix object that can be cached.
makeCacheMatrix <- function(x = matrix()) {
  InvM <- NULL
  set <- function(y) {
    x <<- y
    InvM <<- NULL
  }
  get <- function() x
  set_invmat <- function() InvM <<- solve(x)
  get_invmat <- function() InvM
  list(set = set, get = get,
       set_invmat = set_invmat,
       get_invmat = get_invmat)
}


## Takes as input the result of the makeCacheMatrix method and computes the inverse
## of that matrix while checking for whether the inverse has already been calculated
## and stored in the cache.
## The function checks for whether the cache is empty or not and tests
## that the cached result is the inverse of the matrix passed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvM <- x$get_invmat()
  Ident = diag(dim(mat)[1])
  if(!is.null(InvM)) {
    ## Check to see if cached InvM is the inverse matrix 
    if(x$get %*% InvM == Ident) {
      message("grabbing cache")
      return(InvM) 
    }
  }
  data <- x$get()
  InvM <- solve(data, ...)
  x$setsolve(InvM)
  InvM
}
