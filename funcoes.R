cntdd <- file.path("https://raw.githubusercontent.com",
									 "kleberformiga/contabilidados/master",
									 "contabilidados.R")
source(cntdd)

options(scipen = 999)

cntdd.carregaPacotes("tidyverse")  


eccd.n1.evolucao <- function(codigo = "PETR4", conta = ativo, descricao = "Ativo"){
	
	conta <- enquo(conta)
	
	dados <- bd.eccd.001 %>% filter(cod %in% codigo)
	print(dados)
	
	ggplot(dados, 
				 aes(x = ano, 
				 		y = !!conta,
				 		color = cod)) +
		scale_y_continuous(labels = scales::unit_format(
			unit = "k", 
			scale = 1e-3,
			accuracy = 1, big.mark = ",")) +
		geom_line(size = 1.5, 
							color = "lightgrey") +
		geom_point(size = 3, 
							 color = "steelblue") +
		labs(y = NULL, 
				 x = NULL,
				 title = paste0("Evolução da conta ", descricao, " da empresa ", codigo),
				 subtitle = "Em milhares de reais",
				 caption = paste0("Elaboração: contabiliDados  |", "\n",
				 								 "Fonte: Economatica\U00AE  |")) +
		cntdd.theme
}


eccd.n1.comparaevolucao <- function(codigos = c("MGLU3", "LREN3"), conta = ativo, descricao = "Ativo"){
	
	if(length(codigos) < 2){message("Coloque pelo menos dois códigos")}
	conta <- enquo(conta)
	
	ggplot(data = bd.eccd.001 %>% filter(cod %in% codigos),
				 mapping = aes(x = ano, y = !!conta/1000, color = cod), na.rm = TRUE) +
		geom_line(alpha = .7,
							size  = 2) + 
		labs(title = paste0("Evolução da conta ", descricao, ", por ano"),
				 subtitle = "em milhares R$",
				 caption = paste0("elaborador por contabiliDados", "\n",
				 								 "Fonte: http://www.economatica.com.br/"),
				 x = NULL, 
				 y = NULL,
				 color = "Código") +
		cntdd.theme
}



eccd.n1.relacaoContas <- function(codigo = "VALE3",
																	x = divida, y = ativo,
																	descX = "Dívida",
																	descY = "Ativo",
																	descCod = "VALE"){
	
	
	print(bd.eccd.001 %>% filter(cod == codigo & ano < 2017))
	
	ggplot(bd.eccd.001 %>% filter(cod == codigo & ano < 2017),
				 aes(x = !! enquo(x)/1000,
				 		y = !!enquo(y)/1000,
				 		color = cod), na.rm = TRUE) +
		geom_point(alpha = .7,
							 size  = 4, color = "blue") +
		geom_smooth(method='lm', formula= y~x, show.legend = F) +
		geom_text(aes(label=ano),hjust=-0.3, vjust=0.2, color = "blue", show.legend = F) +
		labs(title = paste0("Relação entre ", descX, "(X) e ", descY, "(Y) da empresa ", descCod),
				 subtitle = "em milhares R$",
				 caption = paste0("elaborado por contabiliDados", "\n", "Fonte: economatica"),
				 x = NULL,
				 y = NULL) +
		cntdd.theme
}

