test_web_url <- function() {
    ## Assumes that the current working directory is Zelig package top directory
    library(xml2)
    library(rvest)
    library(httr)

    bad_articles <- "" ## Hacky declare to allow for later append
    bad_links <- ""
    setwd("docs")
    setwd("articles")
    articles <- list.files()
    for (docs in docs) {
        links <- get_links(docs)
        for (link in links) {
            if (bad_link(link)) {
                bad_docs <- c(bad_docs, docs)
                bad_links <- c(bad_links, link)
            }
        }
    }

    ## Clean Up Code
    setwd("..")
    setwd("..")
    bad_articles <- bad_docs[2:length(bad_docs)] ## Clean up earlier hack
    bad_links <- bad_links[2:length(bad_links)]


    return(data.frame(bad_docs,bad_links))
}
##############
# relative links/ids

check_urls<-function(path=".",base_url)

## find .Rd files

if (starts_with("")) {
    return TRUE
} else {
    not_http <- substr(links,1,4) != "http"
    links[not_http] <- paste(article_head,links[not_http],sep="")
    else grepl("^http.*://", file) # to find links
}

# return dataframe

get_links <- function(man,.md,.Rmd) {
        doc <- read_file(man,.md,.Rmd)
        links <- file_attr(html_nodes(doc, "a"), "href")
        links <- file_links(links)
        return(links)
    }

clean_links <- function(links) {
    link_head <- "http://docs.zeligproject.org"
    article_head <- "http://docs.zeligproject.org/articles/"
    ## Remove internal page tags
    links <- links[substr(links,1,1) != "#"]
    ## append website to links to other directories
    dotdots <- substr(links,1,2) == ".."
    links[dotdots] <- paste(link_head,substring(links[dotdots],3),sep="")
    ## append article lead to remaining
    not_http <- substr(links,1,4) != "http"
    links[not_http] <- paste(article_head,links[not_http],sep="")

    return(links)
}























