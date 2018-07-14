#' Baixa dados de matrix do economatica
#'
#' Mais detalhes
#'
#' @param Nome Nome da variavel
#' @param PathFile Caminho do aquivo
#'
#' @return o valor...
#'
#' @examples
#'
#' @export
BaixaDados <- function(Nome, PathFile, Periodo, Planilha){
  assign(Nome, read_xlsx(PathFile, sheet = Planilha, skip = 1, na = "-"), envir = .GlobalEnv)
  setnames(eval(as.name(Nome)), 1L, Periodo)
  assign(Nome, melt(data.table(eval(as.name(Nome))), id.vars = Periodo, variable.name="cod", value.name = Nome), envir = .GlobalEnv)
}
