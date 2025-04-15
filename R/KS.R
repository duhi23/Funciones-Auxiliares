#' Realiza una prueba de Kolmogorov-Smirnov (KS) entre dos grupos de datos
#'
#' Esta función calcula el valor de la estadística de Kolmogorov-Smirnov (KS) entre dos grupos definidos por la variable `y`.
#' La prueba compara las distribuciones de las dos muestras (una correspondiente a los valores donde `y` es 1 y otra donde `y` es 0)
#' para evaluar si provienen de la misma distribución.
#'
#' @param x Un vector numérico o columna de datos a analizar.
#' @param y Un vector lógico o numérico (con valores 0 y 1) que define los dos grupos de datos a comparar.
#'
#' @return Un valor numérico que representa la estadística de la prueba de Kolmogorov-Smirnov (KS) entre los dos grupos.
#'         Si `x` es un vector de caracteres, la función devuelve 0, ya que la prueba KS no es aplicable a datos no numéricos.
#'
#' @details
#' La prueba de Kolmogorov-Smirnov es una prueba no paramétrica que evalúa si dos muestras provienen de la misma distribución.
#' Se utiliza comúnmente para comparar la forma de las distribuciones de dos grupos de datos.
#' En este caso, los datos de `x` se dividen en dos grupos de acuerdo con los valores de `y`: uno donde `y` es igual a 1 y otro donde `y` es igual a 0.
#' La función devuelve la estadística KS, que mide la mayor diferencia acumulada entre las distribuciones empíricas de los dos grupos.
#'
#' @examples
#' x <- rnorm(100)  # Datos de ejemplo
#' y <- rep(c(1, 0), each = 50)  # Variable de grupo
#' TestKS(x, y)  # Realiza la prueba de KS
#'
#' @export

# Funcion KS
TestKS <- function(x, y){
    if(class(x)!="character"){
        vars <- data.frame(y,x)
        vars_e <- subset(vars,subset=vars[,1]==1)
        vars_f <- subset(vars,subset=vars[,1]==0)
        ks <- suppressWarnings(ks.test(vars_e[,2],vars_f[,2],alternative="two.sided"))
        ks <- round(as.numeric(ks$statistic),4)
    } else{
        ks <- 0
    }
    return(ks)
}

