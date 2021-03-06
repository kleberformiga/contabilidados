#' Transforma dados de matrix do economatica em painel
#'
#' Em pesquisas na area de financas com dados em painel o pesquisador, por vezes,
#' faz estimacoes com dados dispostos em painel. Contudo, as fontes de coleta nem
#' sempre disponibiliza dados no formato painel. Esse formato consiste em dispor
#' os dados em cross-section (i) e tempo (t). Esse codigo tranforma em painel os
#' dados coletados na base Economatica(R), desde que coletadas no template MATRIX.
#' Nesse template os dados sao dispostos em colunas (nao painel) em que cada
#' coluna possui o codigo da empresa para uma mesma variavel. A vantagem desse
#' codigo eh permitir que os dados sejam coletados em um unico arquivo do tipo
#' xlsx em que cada aba contera uma variavel de interesse da pesquisa.
#'
#'
#' @param Nome Nome da variavel. O nome informado sera processado como sendo
#' o nome da variavel no banco de dados e o nome do banco de dados (com letras maiusculas)
#' @param PathFile Caminho do arquivo xlsx.
#' @param Periodo Nome a ser dado a frequencia. Essa informacao fica a criterio
#' do pesquisador para controle de seu banco de dados. Caso os dados coletados
#' sejam trimestrais, pode-se informar #' "trim", "trimestre", "quarter", etc.
#' @param Planilha Informar o numero da aba na qual estao os dados a serem
#' transformados em painel.
#' @param ClassPeriodo Tipo de variavel para periodo do matrix. Se os dados sao
#' trimestreis 1T2018, por exemplo, informar "text". O padrao eh data, pois
#' os matrix, normalmente reportam uma data no periodo.
#' @param ClassValue Tipo de variavel para os valores coletados. Se for setor,
#' por exemplo, colocar "character". Porém a maioria dos valores coletados sao
#' decorrentes de dados financeiros. Por isso o padrao eh "numeric".
#'
#' @return Para melhor desempenho no uso do codigo, todos os matrix deverao ser
#' coletados com as mesmas empresas e o mesmo periodo, permitindo a criacao de
#' um painel balanceado, alem de dar ao pesquisador maior controle sobre os dados
#' coletados e facilitar a juncao de bancos.
#'
#' @seealso \code{\link{data.table}} para manipulacao de dados
#' @seealso \code{\link{readxl}} para importacao de dados xlsx
#' @source  Os dados usados no exemplo estao disponiveis em \url{https://github.com/kleberformiga/contabilidados/blob/master/ExemploEconomatica.xlsx?raw=true}
#'
#' @keywords Data.table, teste
#' @examples
#' Considerando a transformacao da primeira aba do arquivo xlsx na qual esta a
#' variavel ATIVO TOTAL, coletada no matrix na frequencia trimestral, cujo nome
#' escolhido seja "AtTot": disponivel em  #SOURCE:
#'
#' BaixaDados("AtTot", "caminho/exemplo1.xlsx", "trim", 1)
#'
#' Considerando um arquivo com duas ou mais abas (nesse exemplo, duas) em que
#' o pesquisador deseje transformar em painel de forma automatica:
#'
#' dados <- c("precos", "vrmerc") # Vetor com dados de precos e valor de mercado
#'
#' for (i in seq_along(dados)) {
#'    BaixaDados(dados[i], "caminho/nomedoarquivo.xlsx", "trim", i)
#' }
#'
#' Após rodar esse exemplo, as duas variaveis estarao dispostas em painel prontas
#' para serem unidas em um unico banco de dados. O pacote \code{\link{dplyr}} eh
#' recomendado para esse fim.
#'
#' @export
BaixaDados <- function(Nome, PathFile, Periodo, Planilha, ClassPeriodo = "date", ClassValue = "numeric"){
  a <- ncol(read_xlsx(PathFile, sheet = Planilha, skip = 1, na = "-"))
  assign(toupper(paste0("BD", Nome)),
         read_xlsx(PathFile, sheet = Planilha, skip = 1, na = "-",
                   col_types = c(ClassPeriodo, rep(ClassValue, a-1))),envir = .GlobalEnv)
  setnames(get(toupper(paste0("BD", Nome))), 1L, Periodo)
  assign(toupper(paste0("BD", Nome)), melt(data.table(get(toupper(paste0("BD", Nome)))),
         id.vars = Periodo, variable.name="cod", value.name = Nome, variable.factor = F,
         value.factor = F), envir = .GlobalEnv)
}

