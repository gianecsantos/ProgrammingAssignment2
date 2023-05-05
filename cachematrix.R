## Esta função cria um objeto "matriz" especial que pode armazenar em cache seu
## inverso ----

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {   #atualiza a matriz armazenada no objeto
        x <<- y
        i <<- NULL
    }
    get <- function() x   #retorna a matriz armazenada no objeto
    setsolve <- function(solve) i <<- solve   #atualiza a solução da matriz
    getsolve <- function() i   #retorna a solução armazenada
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Esta função recupera o inverso da matriz criada por makeCacheMatrix ou
## calcula e armazena em cache caso a matriz tenha sido alterada.----

cacheSolve <- function(x, ...) {
    i <- x$getsolve()   #atribui a solução da matriz armazenada em cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)   #retorna a solução armazenada em cache, se for encontrada
    }
    data <- x$get()   #recupera a matriz original, se não for encontrada
    i <- solve(data, ...)   #atribui a solução da matriz original
    x$setsolve(i)   #armazena a solução em cache
    i   #retorna a solução da matriz resolvida
}
