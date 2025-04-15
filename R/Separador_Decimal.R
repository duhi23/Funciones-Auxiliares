#' Convertir valores numéricos con coma a punto decimal
#'
#' Esta función convierte un vector de caracteres que representan números con coma (',') como separador decimal a formato numérico estándar, utilizando punto ('.') como separador decimal. Es útil cuando se manejan datos numéricos en formatos donde la coma es el separador decimal (común en muchas regiones europeas).
#'
#' @param vector Un vector de caracteres que contiene números con coma como separador decimal.
#' @return Un vector numérico con los valores convertidos, en el cual las comas se reemplazan por puntos y los caracteres se convierten a tipo numérico.
#'
#' @examples
#' # Ejemplo de uso
#' numeros_coma <- c("12,34", "56,78", "9,10")
#' numeros_convertidos <- val_num(numeros_coma)
#' print(numeros_convertidos)  # Devuelve: 12.34, 56.78, 9.10
#'
#' @export
val_num <- function(vector) {
    res <- as.numeric(sub(pattern = "\\,", replacement = ".", x = vector))
    return(res)
}
