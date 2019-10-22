carregaPacotes <- function (pcts) 
{
  if (!require(pcts, character.only = T)) {
    install.packages(pcts)
    require(pcts, character.only = T, quietly = T)
  }
}


instalaPacotes <- function(x = c("dplyr", "data.table", "tidyverse", "readxl")){
  
  # Informa um vetor com os pacotes a serem instalados
  # Se nada informado, por padrão, instala: dplyr, data.table, tidyverse e readxl
  
  for (i in x) {
    carregaPacotes(i)
  }
}

instalaPacotes()

BaixaDados <- function (Nome, PathFile, Periodo, Planilha, ClassPeriodo = "date", 
                        ClassValue = "numeric") {
  
  # Nome: Informe o nome da variável coletada no matrix do economatica
  # PathFile: Informe o caminho do arquivo xlsx onde está o matrix do economatica
  # Periodo: Informe "trim" para trimestre, "data" para data, "ano" para ano, etc.
  # Planilha: Informe o número da planilha ou nome da aba que contém o matrix
  # ClassPeriodo: Por padrão é formato data, mas se no matrix o período contiver
  #               letras, informar "text" para indicar ser um texto (string)
  # ClassValue: Por padrão é formato numérico, mas se os dados coletados forem
  #             texto (nome do sócio, por exemplo), infomar "text".
  
    a <- ncol(read_xlsx(PathFile, sheet = Planilha, skip = 1, 
                      na = "-"))
  assign(toupper(paste0("BD", Nome)), read_xlsx(PathFile, sheet = Planilha, 
                                                skip = 1, na = "-", col_types = c(ClassPeriodo, rep(ClassValue, 
                                                                                                    a - 1))), envir = .GlobalEnv)
  setnames(get(toupper(paste0("BD", Nome))), 1L, Periodo)
  assign(toupper(paste0("BD", Nome)), melt(data.table(get(toupper(paste0("BD", 
                                                                         Nome)))), id.vars = Periodo, variable.name = "cod", value.name = Nome, 
                                           variable.factor = F, value.factor = F), envir = .GlobalEnv)
}


BaixaDadosReuters <- function (Nome, PathFile, Planilha, RANGE = "P:R", SKIP = 0) 
{
  assign(toupper(Nome), read_xlsx(PathFile, sheet = Planilha, 
                                  skip = SKIP, na = c("-", "NULL", "#N/A"), range = cell_cols(RANGE)), 
         envir = .GlobalEnv)
  setnames(get(toupper(Nome)), 1L:3L, c("cod", "data", Nome))
  setDT(get(toupper(Nome)))
}

