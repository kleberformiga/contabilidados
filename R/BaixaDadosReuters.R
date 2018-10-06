#' Transforma dados de matrix do economatica em painel
#'
#' Em pesquisas na area de financas com dados em painel o pesquisador, por vezes,
#' faz estimacoes com dados dispostos em painel. Contudo, as fontes de coleta nem
#' sempre disponibiliza dados no formato painel. Esse formato consiste em dispor
#' os dados em cross-section (i) e tempo (t). Esse utiliza planilha pre-formatada
#' para coleta de dados na plataforma Eikon da Thomson Reuters relativa a informacoes
#' finaneiras.#'
#'
#' @param Nome Vetor com nomes das variaveis. Os nomes contidos nesse vetor sera
#' o nome da variavel no banco de dados.
#' que aparecera no banco de dados.
#' @param PathFile Caminho do arquivo xlsx.
#' @param Planilha Informar o numero da aba na qual estao os dados a serem
#' transformados em painel.
#' @param RANGE Colunas correspondentes ao intervado contendo o codigo da empresa,
#' a data e o valor da variavel. Por padrao, utiliza-se as colunas "P", "Q" e "R".
#' Portanto, a informacao padrao para o RANGE eh "P:R".
#' @param SKIP Numero de linhas a ser desconsiderada para se ter o inicio do
#' cabecalho dos dados. Por padrao o cabecalho esta na linha 1, portanto o SKIP eh
#' zero.
#'
#' @return Para melhor desempenho no uso do codigo, sugere-se usar a planilha
#' padrao disponibilizada no link disponibilizado na descricao.
#'
#' @seealso \code{\link{data.table}} para manipulacao de dados
#' @seealso \code{\link{readxl}} para importacao de dados xlsx
#' @source  Os dados usados no exemplo estao disponiveis em \url{https://github.com/kleberformiga/contabilidados/raw/master/exemplo1.xlsx}
#'
#' @keywords Data.table, teste
#' @examples
#' <EM CONSTRUCAO>
#'
#' @export

BaixaDadosReuters <- function(Nome, PathFile, Planilha, RANGE = "P:R", SKIP = 0){
  assign(toupper(Nome), read_xlsx(PathFile, sheet = Planilha, skip = SKIP,
                         na = c("-", "NULL", "#N/A"), range = cell_cols(RANGE)),
         envir = .GlobalEnv)
  setnames(get(toupper(Nome)), 1L:3L, c("cod", "data", Nome))
  setDT(get(toupper(Nome)))
}

