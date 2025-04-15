#' Realiza una prueba de Información de Variable (IV) para una variable categórica.
#'
#' @param x Un vector de datos categóricos.
#' @param y Un vector lógico o numérico que representa la variable dependiente o de respuesta.
#'
#' @details
#' Esta función calcula el valor de la Información de Variable (IV) utilizando la relación entre las frecuencias relativas de las clases de la variable independiente `x` y las clases de la variable dependiente `y`. El IV se usa comúnmente en modelos de clasificación, especialmente en el contexto de variables predictoras categóricas.
#' La fórmula utilizada considera la log-odds de las probabilidades de que los valores de `x` se asocien con las clases 0 y 1 de `y`, asignando pequeños valores a probabilidades cercanas a 0 o 1 para evitar problemas con el logaritmo de cero.
#'
#' @return
#' Un valor numérico que representa el valor de Información de Variable (IV) para la variable categórica `x`.
#'
#' @examples
#' # Ejemplo de uso:
#' x <- factor(c('A', 'B', 'A', 'A', 'B', 'C', 'C', 'B'))
#' y <- c(1, 0, 1, 1, 0, 1, 0, 1)
#' TestVI(x, y)
#'
#' @export

TestVI <- function(x,y){
    if(class(x)=="character"){
        tc <- table(y,x)
        f1 <- tc[1,]
        f2 <- tc[2,]
        aux1 <- ifelse(f1/sum(f1)==0,0.001,ifelse(f1/sum(f1)==1,0.999, f1/sum(f1)))
        aux2 <- ifelse(f2/sum(f2)==0,0.001,ifelse(f2/sum(f2)==1,0.999, f2/sum(f2)))
        wof <- log(aux2/aux1)
        wof <- ifelse(wof==-Inf,0,wof)
        VI <-   sum(((f2/sum(f2))-(f1/sum(f1)))*wof)
    }else{
        VI <- 0
    }
    return(VI)
}

