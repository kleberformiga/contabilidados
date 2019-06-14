verH5 <- function(search){

  #Specifying the url for desired website to be scrapped
  url <- paste0("https://scholar.google.com/citations?hl=pt-BR&view_op=search_venues&vq=", search, "&btnG=")

  #Reading the html content from Scholar
  webpage <- read_html(url)

  #scrape title of the product
  title_html <- html_nodes(webpage, "table#gsc_mvt_table")
  title <- html_table(title_html)
  df <- Reduce(merge, title)
  df <- df[, 2:4]
  return(df)
}
