## These functions will cache a special matrix inverse 
## and solve the matrix if it has yet to be solved. 


## This function creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  sol = NULL
  
  get = function(){x}
  
  setsolve = function(solve){ sol <<- solve}
  
  getsolve = function(){sol}
  
  list( get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special matrix returned by the makeCacheMatrix above
## If the inverse has already been calculated(and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sol = x$getsolve()
  
  if(!is.null(sol)){
    
    message("getting cached data")
    return(sol)
    
  }
  data = x$get()
  
  sol = solve(data)
  
  x$setsolve(sol)
  
  sol
  
}

