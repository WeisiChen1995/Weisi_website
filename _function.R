library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)
library(gsheet)

make_doi <- function(doi) {
  return(glue::glue('DOI: [{doi}](https://doi.org/{doi})'))
}

make_citation <- function(pub) {
  if (!is.na(pub$journal)) {
    pub$journal <- glue::glue('_{pub$journal}_.')
  }
  if (!is.na(pub$number)) {
    pub$number <- glue::glue('{pub$number}.')
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }
  pub$year <- glue::glue("({pub$year})")
  pub$title <- glue::glue('"{pub$title}"')
  pub[,which(is.na(pub))] <- ''
  return(paste(
    pub$author, pub$year, pub$title, pub$journal, 
    pub$number, pub$doi
  ))
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

get_pubs <- function() {
  pubs <- gsheet::gsheet2tbl(
    url = 'https://docs.google.com/spreadsheets/d/1OVaeVaNed3mwQlL8u6sXU1TMbn6J8WeFAqvY-CZlIRI/edit?gid=0#gid=0')
  pubs <- make_citations(pubs)
  pubs$summary <- ifelse(is.na(pubs$summary), FALSE, pubs$summary)
  pubs$stub <- make_stubs(pubs)
  pubs$url_summary <- file.path('research', pubs$stub, "index.html")
  
  # Bold "Chen, W." in the authors column
  pubs$author <- gsub("Chen, W", "<b>Chen, W</b>", pubs$author)
  
  return(pubs)
}

make_pub_list <- function(pubs, category) {
  x <- pubs[which(pubs$category == category),]
  pub_list <- list()
  for (i in 1:nrow(x)) {
    pub_list[[i]] <- make_pub(x[i,], index = i)
  }
  return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}

make_stubs <- function(pubs) {
  journal <- str_to_lower(pubs$journal)
  journal <- str_replace_all(journal, ':', '')
  journal <- str_replace_all(journal, '`', '')
  journal <- str_replace_all(journal, "'", '')
  journal <- str_replace_all(journal, "\\.", '')
  journal <- str_replace_all(journal, "&", '')
  journal <- str_replace_all(journal, ',', '')
  journal <- str_replace_all(journal, '  ', '-')
  journal <- str_replace_all(journal, ' ', '-')
  return(paste0(pubs$year, '-', journal))
}




