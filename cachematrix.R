
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makecachematrix function that creates objects inside matrix that can inverses.
# cachesolve: computes the inverse
makeCacheMatrix <- function(x = matrix()) {
  inversus <- NULL #starting the inverse as null
  set<- function(y){
    x<<-y
    inversus<<- NULL
  }
  get<-function(){x} #function to get the matrix
  setinversus<-function(inversuscalculus){inversus<<-inversuscalculus}
  getinversus<-function() {inversus}
  list(set = set, get = get,
       setinversus = setinversus,
       getinversus = getinversus)
}


## Write a short comment describing this function
# used to get the cache data
cacheSolve <- function(x, ...) { #get the cache data
  inversus <-x$getinversus()
  if(!is.null(inversus)) { #checks if the value is null
    message("getting cached data") 
    return(inversus) #returns inverse values
  }
  
  data<- x$get()
  inversus <- solve(data, ...) #calculates inverse
  x$setinversus(inversus)
  inversus
  ## Return a matrix that is the inverse of 'x'
}
