#' Coleta dados de planilha gerada pela base Thomson Reuters (Eikon)
#'
#' Em pesquisas na area de financas com dados em painel o pesquisador, por vezes,
#' faz estimacoes com dados dispostos em painel. Esse formato consiste em dispor
#' os dados em cross-section (i) e tempo (t). Essa funcao utiliza a planilha pre-formatada
#' para coleta de dados em painel na plataforma Eikon da Thomson Reuters relativa a informacoes
#' financeiras.
#'
#' @param Nome Nome da variavel. O nome informado sera processado como sendo
#' o nome da variavel no banco de dados e o nome do banco de dados (com letras maiusculas)
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
#' @source  Os dados usados no exemplo estao disponiveis em \url{https://github.com/kleberformiga/contabilidados/blob/master/ExemploEikon.xlsx?raw=true}
#'
#' @keywords Data.table, teste
#' @examples
#' O uso natural da funcao eh:
#' BaixaDadosReuters("AtivoTotal", "caminho do xlsx", 1)
#'
#' Nesse caso, importaria o vetor Ativo Total disponivel na planilha indicada no caminho
#' informado, dentro da primeira aba (1).
#' Adicionalmente, eh possivel coletar, por meio dessa funcao, duas ou mais variaveis e
#' concatena-las em um mesmo banco. Para fins de exemplo, a planilha "ExemploEikon.xlsx"
#' contem apenas das variaveis: Ativo Total e Receita. O codigo abaixo importa ambas as
#' variaveis e as une.
#'
#' A planilha eh assim formatada:
#' A1 - Numero de codigos coletados dispostos a partir da celula A4
#' A4:A511 - Lista dos tickers de cada empresa a ser pesquisada
#' C4 - Codigo da variavel de interesse na base Thomson Reuters
#' C5 - Formula contendo a descricao da variavel
#' D1:D3 - Formulas da descricao, dados anuais e dados trimestrais, respectivamente
#' P:R - Colunas que recebem os dados anuais
#' U:W - Colunas que recebem os dados trimestrais
#'
#' Essa funcao, por padrao, esta configurada para os dados anuais, podendo o usuario
#' apenas alterar o parametro RANGE para U:W, caso queira coletar trimestral.
#' Para iniciar o codigo, cria-se um vetor chamado "dados", composto pelo nome de cada variavel,
#' na ordem em que sao dispostas nas abas da planilha. O codigo deve ser assim executado:
#'
#' dados <- c("Receita", "AtivoTotal")
#'
#'# for (i in seq_along(dados)) {
#'   if(i==1){AuditaBD <- data.frame(Variavel = NA, codigo = NA, Descricao = NA)}
#'   BaixaDadosReuters(dados[i], Arquivo, i);
#'   setnames(get(dados[i]), 1L:3L, c("cod", "data", dados[i]));
#'   AuditaBD[i,1] = dados[i]; AuditaBD[i, 2] = names(data.table(read_xlsx(Arquivo, sheet = i, skip = 0, na = "-", range = "C4:C5")));
#'   AuditaBD[i, 3] = names(data.table(read_xlsx(Arquivo, sheet = i, skip = 0, na = "-", range = "C5:C6")))
#'   if(i==length(dados)){assign("BDVetor", plyr::join_all(mget(dados), by = index, type = "full"), envir = .GlobalEnv)}
#' }
#'
#' Ao final tem-se um banco de dados com todas as variaveis chamado "BDVetor" e outro
#' com o nome "AuditaBD". Esse ultimo contem uma lista de todas as variaveis coletadas
#' com o respectivos codigos. A finalidade eh auditar se a informacao prestada no vetor
#' dados corresponde a variavel efetivamente coletada na planilha.
#'
#' @export

BaixaDadosReuters <- function(Nome, PathFile, Planilha, RANGE = "P:R", SKIP = 0){
  assign(toupper(Nome), read_xlsx(PathFile, sheet = Planilha, skip = SKIP,
                         na = c("-", "NULL", "#N/A"), range = cell_cols(RANGE)),
         envir = .GlobalEnv)
  setnames(get(toupper(Nome)), 1L:3L, c("cod", "data", Nome))
  setDT(get(toupper(Nome)))
}

