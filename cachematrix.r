## makeCacheMatrix function to cache the inversion of a matrix
## cacheSolve function to retrieve inverse from the cache or set the inversion value to cache if it not already been calculated  
## makeCacheMatrix creates a list containing functions to 
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse
## 4.get the inverse
## this list is then used as the input to cacheSolve()
 
makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set = function(y){
      x<<-y
      i<<-NULL
      }
        
    get = function() x
    seti = function(inverse) i <<- inverse
    geti = function() i
    list(set = set, get = get, seti = seti, geti = geti)
    }

 
## take in output of makecacheMatrix
## cacheSolve is a function which will check if matrix already
## inversed. If yes, it gets the inverse from the cache and skips
## computation 

cacheSolve <- function(x, ...) {
      i = x$geti()
      ## check if inverse has already been calculated
      if(!is.null(i)){
          message("getting cached data")
          return(i)
          }
      
      ## else, calculates the inverse
      data = x$get()
      i = solve(data,...)
    
      ## set inverse value in cache
      x$seti(i)
      
      ## Return a matrix that is the inverse of 'x'
        return(i)
        }