## Inverting matrices can be a time and computationally intensive task. 
## These functions are intended to cut down on the effort needed by
## caching the inverse of a square matrix and calling that cached
## value if available rather than computing it every time an inverse
##  is needed.



## makeCacheMatrix takes as its input a square matrix and computes
## and caches its inverse for later retrieval and usage.

makeCacheMatrix <- function(x = matrix()) {
     
     my_matrix <- NULL
     my_cache <- NULL
     
     if(!is.matrix(x)){
          print("Input is not a matrix. Please provide a matrix")
          return(invisible())
     }
     
     if(class(as.vector(x)) != "numeric") {
          print("Please provide a numeric vector")
          return(invisible())
     }
     
     if(is.na(sum(colSums(x)))) {
          print("Matrix contains NULL or empty values. Please provide a filled matrix")
          return(invisible())
     }
     
     if(!identical(nrow(x), ncol(x))) {
          print("Matrix is not square. Please provide a square matrix")
          return(invisible())
     }
     my_matrix <<- x
     my_cache <<- solve(x)

}


## cacheSolve takes as its input a matrix and returns its inverse.
## If the supplied matrix has already been supplied to the makeCacheMatrix
## function, it will return the cached inverse. If not, it will compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     if(!is.matrix(x)){
          print("Input is not a matrix. Please provide a matrix")
          return(invisible())
     }
     if(is.na(sum(colSums(x)))){
          print("Matrix contains NULL or empty values. Please provide a filled matrix")
          return(invisible())
     }
     if(!identical(nrow(x), ncol(x))){
          print("Matrix is not square. Please provide a square matrix")
          return(invisible())
     }
     if(exists("my_matrix")) {
          if(identical(x, my_matrix)){
               print("Retrieving cached value")
               return(my_cache)
          }
     }
     solve(x)
}
