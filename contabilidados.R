CarregaPacotes <- function (pcts) 
{
  if (!require(pcts, character.only = T)) {
    install.packages(pcts)
    require(pcts, character.only = T, quietly = T)
  }
}

BaixaDados <- function (Nome, PathFile, Periodo, Planilha, ClassPeriodo = "date", 
                        ClassValue = "numeric") 
{
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

