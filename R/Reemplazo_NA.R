#' Reemplazar valores NA en columnas especificadas
#'
#' Esta función reemplaza los valores NA en las columnas de un data.table con un valor especificado.
#'
#' @param dt un objeto data.table
#' @param vars un vector de nombres de columnas a procesar
#' @param valor el valor con el que reemplazar los NAs
#'
#' @return El data.table con los NAs reemplazados
#' @export


reemplazo_col <- function(dt, vars, valor) {
    # Verificar que dt sea un data.table
    if (!data.table::is.data.table(dt)) {
        dt <- data.table::as.data.table(dt)
    }

    # Verificar que las columnas existan en el data.table
    cols_no_existentes <- vars[!vars %in% names(dt)]
    if (length(cols_no_existentes) > 0) {
        stop("Las siguientes columnas no existen en el data.table: ",
             paste(cols_no_existentes, collapse = ", "))
    }

    # Reemplazar NA en cada columna especificada
    for (var in vars) {
        set(dt, which(is.na(dt[[var]])), var, valor)
    }
}
