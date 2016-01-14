
##makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  Get <- function() x
  SetInverseMatrix <- function(y)
  {
    InverseMatrix <<-y
  }
  GetInverseMatrix <- function() InverseMatrix
  list(Get = Get,SetInverseMatrix = SetInverseMatrix,GetInverseMatrix = GetInverseMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  result <- x$GetInverseMatrix()
  if (is.null(result))
  {
    temp <-x$Get()
    result <- solve(temp)
    x$SetInverseMatrix(result)
    result
  }
  else
    result
}
