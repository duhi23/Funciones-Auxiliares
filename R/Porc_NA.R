#' Análisis de variables constantes
#'
#' @param x Variables Numericas
#' @export

porcNA <- function(x){
    porc <- mean(is.na(x))
    return(porc)
}
