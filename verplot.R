verPlot <- function(setor){
  ggplot(dados[econBov %in% as.character(setores[c(setor)]), .(econBov, trim2, divAtivo, plAtivo)], aes(trim2 , value, color = variable)) +
    geom_line(aes(y = divAtivo, col = "divAtivo")) +
    geom_line(aes(y = plAtivo, col = "plAtivo")) +
    ggtitle(paste0("Fonte de financiamento do setor \n", setores[c(setor)])) +
    xlab("Valor em percentual") + ylab("Trimestres") +
    labs(color="Variáveis") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = "bottom")
}
