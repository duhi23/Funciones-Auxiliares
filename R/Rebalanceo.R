#' Rebalanceo de clases desbalanceadas
#'
#' Esta función realiza un rebalanceo de clases en un conjunto de datos, generando un muestreo con reemplazo de la clase minoritaria (en este caso, la clase 1).
#' El objetivo es igualar las proporciones de ambas clases en la variable dependiente.
#'
#' @param data Un data frame que debe contener una variable dependiente llamada `VarDep` con valores 0 y 1, donde 0 es la clase mayoritaria y 1 la minoritaria.
#' @param porc El porcentaje de la clase minoritaria que deseas tener después del rebalanceo. Este valor debe estar entre 0 y 1.
#'
#' @return Un data frame con las clases rebalanceadas.

#' @export

rebalanceo <- function(data, porc) {
    ng <- which(data$VarDep == 0)
    nb <- which(data$VarDep == 1)
    perc <- length(nb) / (length(nb) + length(ng))
    nperc <- floor(porc * length(nb) / perc)
    set.seed(2468)
    nb <- sample(nb, size = nperc, replace = TRUE)
    nr <- c(ng, nb)
    return(data[nr, ])
}
