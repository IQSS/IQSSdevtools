## Test script for link cleaner
## build and reload IQSS devtools first
source("R/check_doc_links.R")
get_html_links1 <- function(html_doc, base_url = "") {
    doc <- read_html(html_doc)
    links <- html_attr(html_nodes(doc, "a"), "href")
    links <- clean_links1(links, base_url)
    return(links)
}


get_md_links1 <- function(path, base_url = "") {
    doc_str <- markdownToHTML(path)
    links <- get_html_links1(doc_str)
    return(links)

}

get_rd_links1 <- function(path, base_url) {
    links <- vector()
    doc_str <- readLines(path)
    doc_str <- unlist(strsplit(doc_str, " "))
    doc_str <- doc_str[grep("url\\{.*\\}", doc_str)]
    doc_str <- gsub("^.*\\{", "", doc_str)
    doc_str <- gsub("\\}.*$", "", doc_str)
    links <- gsub("\\\\\\\\%20", "\\%20", doc_str)
    links <- clean_links1(links, base_url)

    return(links)
}

parse_file1 <- function(path, base_url) {
    if (missing(base_url)) base_url <- ""
    if (file_ext(path) == "html") {
        links <- get_html_links1(path, base_url = base_url)
    } else if (file_ext(path) == "Rd") {
        links <- get_rd_links1(path, base_url = base_url)
    } else if (file_ext(path) == "md"){
        links <- get_md_links1(path, base_url = base_url)
    } else {
        links <- character(0)
    }
    return(links)
}

clean_links1 <- function(links, base_url = "") {
    out <- list()
    ## Allow original link to stay for dataframe
    if (base_url[1] != "") base_url <- c("",base_url)
    ## Remove trailing /
    if (!missing(base_url)) base_url <- gsub("/$", "", base_url)
    ## Remove internal page tags
    links <- links[substr(links, 1, 1) != "#"]
    #Clean Relative links
    for (link in links) {
       if(substr(link, 1, 2) == ".."){
         out <- append(out,list(paste0(base_url, substring(link, 3))))
       } else if (substr(link, 1, 4) != "http") {
    ## append article lead to remaining
         out <- append(out, list(paste0(base_url, "/", link)))
       } else {
          out <- append(out, link)
       }
    }

    return(out)
}

try_links <- function(links) {
    bad_links <- vector()
    for (u in links) {
        if (u == "") next
        link_error <- tryCatch(get_error(u), error = function(e) e)
        if (any(class(link_error) == "error")) link_error <- TRUE
        if (link_error) {
            bad_links <- c(bad_links, u)
        }
    }
    return(bad_links)
}

## list data structure link tester
try_links1 <- function(links) {
    bad_links <- vector()
    for (u in links) {
        good <- FALSE
        if (u[1] == "") next
        for (i in u) {
            ##browser()
            link_error <- tryCatch(get_error(i), error = function(e) e)
            if (any(class(link_error) == "error")) link_error <- TRUE
            if (link_error) {
                next
            } else {
                good <- TRUE
                break
            }
        }
        if (!good) bad_links <- c(bad_links, u[1])
    }

    return(bad_links)
}



test1 <- function() {
    path <- "~/Documents/IQSS_Work/Zelig"
    bad_links <- vector()
    base_url <- c("http://docs.zeligproject.org","http://docs.zeligproject.org/articles")
    search_dirs <- c(path, "~/Documents/IQSS_Work/Zelig/man", "~/Documents/IQSS_Work/Zelig/docs", "~/Documents/IQSS_Work/Zelig/docs/articles", "~/Documents/IQSS_Work/Zelig/docs/news",
                     "~/Documents/IQSS_Work/Zelig/docs/reference")
    articles <- get_file_list(search_dirs)
    message('Checking URLs in:')
    for (i in articles) {
        message(i)
        links <- parse_file(i, base_url)
        bad_links <- c(bad_links, try_links(links))
    }
    return(bad_links)
}

test2 <- function() {
    message("Testing multi base")
    bad_links <- vector()
    path <- "~/Documents/IQSS_Work/Zelig"
    base_url <- c("http://docs.zeligproject.org","http://docs.zeligproject.org/articles")
    search_dirs <- c(path, "~/Documents/IQSS_Work/Zelig/man", "~/Documents/IQSS_Work/Zelig/docs", "~/Documents/IQSS_Work/Zelig/docs/articles", "~/Documents/IQSS_Work/Zelig/docs/news",
                     "~/Documents/IQSS_Work/Zelig/docs/reference")
    articles <- get_file_list(search_dirs)
    message('Checking URLs in:')
    for (i in articles) {
        message(i)
        links <- parse_file1(i, base_url)
        bad_links <- c(bad_links, try_links1(links))
    }
    return(bad_links)


}
##print(try_links(clean_links(test_links,base_url)))
##print(try_links1(clean_links1(test_links,base_url)))
