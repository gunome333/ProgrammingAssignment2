## Coursera R Programming - programming assignment #2
## R function to cache potentially time-consuming computations:
## Here: "write a pair of functions that cache the inverse of a matrix."

## This assignment is not what it appears. We are given example code for the
## analogous makeVector and cachemean. As a matrix is just a generalization of a
## vector, and matrix inversion is a through a native function ( solve() ) just
## like mean() is, the process of cacheing and computing is unchanged. Therefore,
## it is easy to produce the correct functions just by replacing 'vector' with
## 'matrix' and 'mean' with 'solve,' in the example code.

## I think the assignment is really to figure out how the code works and then to comment 
## these explanations into the submitted functions.

## ***********************************

## makeCacheMatrix takes input defining a matrix and sets up a list with elements
## that can store information regarding the matrix, and which can be called later
## by other functions. A Cache is alocated to hold values.

makeCacheMatrix <- function( x = matrix() ){ #input must be matrix || try coerce
      i <- NULL #  variable i (inverse) for later query tests is set to null 
            #this is the variable that will hold the matrix solution

      set <- function(y) { #reset matrix input then make sure i is null
            x <<- y
            i <<- NULL
      }

      get <- function() x # return matrix input
      setsolve <- function(solution) i <<- solution # i is set to the calculated solution matrix
      getsolve <- function() i # return i
      print("cache has been set up and the list of functions is defined for it")
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # generates a list
            #with 4 elements each recursively defined. if function is called list is returned.
}


## cacheSolve solves and returns (finds the inverse of, solve() ) a matrix after
## checking that solve() hasn't previously been done on the same matrix and stored
## in cached location. if solve HAS already been calculated the function returns
## the previously stores matrix solution.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x', input should be list
      ##  First check to see if inverse (i) has already been defined.
      i <- x$getsolve() #check whether the solution matrix i has been calculated already
      if(!is.null(i)) { #i is null be default, if i is not still null then return cached data
            message("getting cached data")
            return(i) # return and do not proceed in function further
      }

      ##  if i is null, solve the matrix (data) and set cache i to solution
      data <- x$get() # get matrix by calling function that returns it.
      i <- solve(data, ...) #solve the matrix and define local i
      x$setsolve(i) #set the cache solution to be !null and = i
      i # return matrix inversion
}