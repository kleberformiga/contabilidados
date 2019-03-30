#' Carrega pacotes requeridos
#'
#' Verifica se o pacote requerido estah instalado. Caso nao esteja, instala e
#' carrega o pacote requerido.
#'
#' @param pcts Nome do pacote requerido.
#'
#' @return Os dados fornecidos devem ser do tipo character, portanto, deve ser
#' informado entre aspas.
#'
#' @examples
#' O uso natural da funcao eh:
#' CarregaPacotes("dplyr")
#'
#' Nesse caso, verificaria se o pacote dplyr estaria instalado. Caso positivo,
#' carregaria o pacote. Caso negativo, instalaria e, posteriormente a instalacao,
#' o carregaria.
#'
#' Para um uso mais geral, recomenda-se o uso de um vetor de pacotes requeridos
#' para desenvolver o trabalho. Nesse caso, poderia utilizar o seguinte codigo:
#'
#' pacotes <- c("tidyverse", "data.table", "readxl") # Vetor com tres pacotes
#'
#' for(i in pacotes){CarregaPacotes(i)} # Verifica todos os pacotes informados
#'
#' Ao final tem-se todos os pacotes informados instalados e/ou carregados.
#'
#' @export

CarregaPacotes <- function(pcts){
  if(!require(pcts, character.only = T)){install.packages(pcts);
    require(pcts, character.only = T, quietly = T)}
}
