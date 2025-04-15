#' Eliminar variables con alta correlación
#'
#' Esta función elimina una o más variables de un conjunto de datos que tienen una correlación superior al umbral especificado con otras variables. La idea es reducir la multicolinealidad en los datos eliminando aquellas variables que están fuertemente correlacionadas entre sí.
#'
#' @param data data.table o data.frame: Conjunto de datos en el que se calcularán las correlaciones entre las variables.
#' @param corr.max numérico (valor predeterminado: 0.75): Umbral de correlación máxima permitido entre dos variables. Si el valor absoluto de la correlación entre dos variables es mayor o igual a este valor, una de las variables se eliminará.
#'
#' @return Un conjunto de datos (data.table o data.frame) en el que se han eliminado las variables con alta correlación.
#'
#' @details La función calcula la matriz de correlación entre las variables del conjunto de datos y busca pares de variables cuya correlación absoluta sea mayor o igual que el valor de `corr.max`. De estos pares, se eliminará una de las variables, priorizando la variable de mayor índice. La función devuelve el conjunto de datos con las variables seleccionadas sin la eliminación.
#'
#' @examples
#' # Ejemplo con un conjunto de datos de prueba
#' data <- data.table(a = rnorm(100), b = rnorm(100), c = rnorm(100))
#' data$c <- data$a + rnorm(100)  # Introduce una correlación alta entre 'a' y 'c'
#' data_limpia <- DVarCorr(data, corr.max = 0.8)
#'
#' @export


DVarCorr <- function(data, corr.max = 0.75){
    COR.AUX <- cor(data)
    pos <- which(((abs(COR.AUX)>=corr.max) & (row(COR.AUX) < col(COR.AUX))), arr.ind=T)
    if(nrow(pos)>0){
        col_elim <- numeric(nrow(pos))
        for(i in seq(1:nrow(pos))){
            aux_col_elim <- c(pos[i,1],pos[i,2])
            if (!any(col_elim %in% aux_col_elim)){
                col_elim [i] <- pos[i,which.max(c(pos[i,1],
                                                  pos[i,2]))]
            }
        }
        if(length(col_elim)>0){
            col_elim <- unique(col_elim[col_elim>0])
            vars <- names(data)[-(col_elim)]
            data <- data.frame(data[,-(col_elim)])
            colnames(data) <- vars
        }
    }
    return(data)
}
