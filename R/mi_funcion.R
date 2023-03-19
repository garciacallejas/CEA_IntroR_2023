
#' titulo de la funcion
#' descripcion detallada de lo que hace la funcion
#'
#' @param x valor numerico
#' @param y valor numerico
#'
#' @return valor numerico, resultado de aplicar sqrt(x^2 + y^2)
#' @export
#'
#' @examples
#' mi_funcion(2,3)
mi_funcion <- function(x,y){
  
  # -------------------------------------------------------------------------
  # cuerpo de la función
  
  # si x es numero -> 
  if(is.numeric(x)){
    resultado <- sqrt(x^2 + y^2)
  }else{
    resultado <- NA
  }
# -------------------------------------------------------------------------
# retorno  
  return(resultado)
}


