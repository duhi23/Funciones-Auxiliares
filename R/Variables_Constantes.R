#' Análisis de variables constantes
#'
#' @param x Variables Numericas
#' @export

constante <- function(x){
    if(class(x)=="numeric"){
        cte <- min(x, na.rm = TRUE)==max(x, na.rm = TRUE)
    } else {
        tc <- prop.table(table(x))>=0.99
        cte <- any(tc)
    }
    return(cte)
}
