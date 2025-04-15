#' @title Calcular diferencia en meses entre dos fechas
#' @description Esta función calcula la diferencia en meses entre una fecha inicial (`start_date`) y una fecha final (`end_date`). Es útil para medir el tiempo transcurrido entre dos puntos en el tiempo, por ejemplo, para evaluar el desempeño de una persona a lo largo de varios meses.
#'
#' @param end_date Fecha final (de tipo `Date`, `POSIXt` o cadena de texto en formato "YYYY-MM-DD").
#' @param start_date Fecha inicial (de tipo `Date`, `POSIXt` o cadena de texto en formato "YYYY-MM-DD").
#'
#' @return Un valor numérico que representa la diferencia en meses entre las dos fechas.
#'
#' @details La función calcula la diferencia en meses utilizando la diferencia entre los años y los meses de las fechas de inicio y fin. La unidad de medida es el mes completo (sin considerar los días exactos). Esto puede ser útil cuando se necesita calcular periodos de tiempo sin tener que preocuparse por días exactos o fracciones de meses.
#'
#' @examples
#' # Ejemplo de uso
#' start_date <- "2023-01-15"
#' end_date <- "2024-01-15"
#' num_meses(start_date, end_date)  # Devuelve 12 meses
#'
#' # Diferencia de 6 meses
#' start_date <- "2023-06-01"
#' end_date <- "2023-12-01"
#' num_meses(start_date, end_date)  # Devuelve 6 meses
#'
#' @export

num_meses <- function(end_date, start_date){
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    mes <- floor(12 * (ed$year - sd$year) + (ed$mon - sd$mon))
    return(mes)
}
