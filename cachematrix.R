# ****************************************************************
# Funcions para invertir cualquiel mastriz cuadrada y almacenarla
# en memoria para su uso posterior.
# ****************************************************************

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 

  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function(){ x }
  
  setinv <- function(i){ m <<- i }
  
  getinv <- function(){ m }  
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)    
}

# ****************************************************************
# Instale el paquete MASS con : 
#                               install.packages("MASS",dep=TRUE)
# Cargar el paquete MASS con : 
#                              library(MASS)
#
# ****************************************************************

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Obteniendo los datos en cache")    
  }else{
    data <- x$get()
    m <- ginv(data, ...)
    x$setinv(m)
  }
  return(m)  
}

# ************************************************************
#  Ejemplo
# ************************************************************

# matriz <- makeCacheMatrix(matrix(rnorm(25)*10,5,5))
# cacheSolve(matriz)
# cacheSolve(matriz)
