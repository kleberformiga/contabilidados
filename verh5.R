verH5 <- function(search){

  journal <- sub("&", "%26", search) # Ajusta o & comercial para o codigo real
  journal <- sub("@", "%40", journal) # Ajusta o @ arroba para o codigo real
  journal <- sub(" *\\(.*", "", journal) # não inclui a partir de qualquer parentese
  journal <- sub("\\.", "", journal) # retirar pontos
  journal <- sub(":", "", journal) # retirar dois pontos
  journal <- dQuote(str_replace_all(journal, " ", "+")) # coloca sempre entre aspas

  #Specifying the url for desired website to be scrapped
  url <- paste0("https://scholar.google.com/citations?hl=pt-BR&view_op=search_venues&vq=", journal, "&btnG=")

  #Reading the html content from Scholar
  webpage <- read_html(url)

  #scrape title of the product
  title_html <- html_nodes(webpage, "table#gsc_mvt_table")
  title <- html_table(title_html)
  df <- Reduce(merge, title)
  df <- df[, 2:4]
  return(df)
}
