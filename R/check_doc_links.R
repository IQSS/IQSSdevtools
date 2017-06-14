#' Determine if URLs in a package's documentation are valid
#'
#' @param path A path pointing to an R package, or a directory containing R
#'    documentation. The default is the current directory.
#' @inheritParams clean_links
#'
#' @examples
#' check_doc_links()
#'
#' @importFrom devtools as.package
#' @export

check_doc_links <- function(path = ".", base_url) {
    if (!dir.exists(path))
        stop(sprintf('%s is not an available directory', path), call. = FALSE)

    start_path <- getwd()
    if (path != ".") setwd(path)

    bad_articles <- bad_links <- vector()

    test_pack <- tryCatch(as.package(path), error = function(e) e)
    is_package <- !any(class(test_pack) == "error")
    if (is_package) {
        search_dirs <- c(path, "man", "docs", "docs/articles", "docs/news",
                         "docs/reference")
        if (missing(base_url)) {
            message("\nNo base URL supplied. Relative links will be listed as broken.\n")
        }
    } else {
        search_dirs <- path
        base_url <- ""
    }
    articles <- get_file_list(search_dirs)

    message('Checking URLs in:')
    for (i in articles) {
        message(i)
        links <- parse_file(i, base_url)
        for (u in links) {
            if (u == "") next
            link_error <- tryCatch(get_error(u), error = function(e) e)
            if (any(class(link_error) == "error")) link_error <- TRUE
            if (link_error) {
                bad_articles <- c(bad_articles, i)
                bad_links <- c(bad_links, u)
            }
        }
    }
    out <- data.frame(file = bad_articles, URL = bad_links,
                      stringsAsFactors = FALSE)

    ## Reset working directory
    if (path != ".") setwd(start_path)

    if (nrow(out) == 0) {
        message("\n\nSuccess! All URLs are working.\n")
    }
    else {
        message("\n\nBad URLs found\n")
        return(out)
    }

}

#' Error in GET call
#'
#' @param URL
#'
#' @importFrom httr status_code GET

get_error <- function(URL) {
    if(status_code(GET(URL)) %in% c(400, 404))
        return(TRUE)
    else
        return(FALSE)
}

#' Parses an html document and returns a vector of all links in the document
#'
#' @param html_doc character string path to an html file
#' @inheritParams clean_links
#'
#' @author Ben Sabath
#' @return a vector of urls
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_attr html_nodes

get_html_links <- function(html_doc, base_url = "") {
    doc <- read_html(html_doc)
    links <- html_attr(html_nodes(doc, "a"), "href")
    links <- clean_links(links, base_url)
    return(links)
}

#' Extract links from Rd documentation files
#'
#' @param path character string path to an Rd R documentation file.
#' @inheritParams clean_links


get_rd_links <- function(path, base_url) {
    links <- vector()
    doc_str <- readLines(path)
    doc_str <- unlist(strsplit(doc_str, " "))
    doc_str <- doc_str[grep("url\\{.*\\}", doc_str)]
    doc_str <- gsub("^.*\\{", "", doc_str)
    doc_str <- gsub("\\}.*$", "", doc_str)
    links <- gsub("\\\\\\\\%20", "\\%20", doc_str)
    links <- clean_links(links, base_url)

    return(links)
}

#' Parse markdown files to HTML and extract URLs
#'
#' @param path character string path to a markdown file.
#' @inheritParams clean_links
#'
#' @importFrom markdown markdownToHTML

get_md_links <- function(path, base_url = "") {
    doc_str <- markdownToHTML(path)
    links <- get_html_links(doc_str)
    return(links)

}

#' Prepares relative links for checking
#'
#' @param links vector of URLs
#' @param base_url a character string for a domain for which relative links are
#'    subdomains. Used to resolve relative URL paths in documentation.

clean_links <- function(links, base_url = "") {
    ## Remove trailing /
    if (!missing(base_url)) base_url <- gsub("/$", "", base_url)
    ## Remove internal page tags
    links <- links[substr(links, 1, 1) != "#"]
    #Clean Relative links
    dotdots <- substr(links, 1, 2) == ".."
    links[dotdots] <- paste(base_url, substring(links[dotdots], 3), sep = "")
    ## append article lead to remaining
    not_http <- substr(links, 1, 4) != "http"
    links[not_http] <- paste(base_url, "/", links[not_http], sep = "")

    return(links)
}

#' Gathers documenttion files paths into a single vector
#'
#' @param search_dirs vector of paths to directories to search for
#'    documentation.
#'
#' @importFrom tools list_files_with_exts

get_file_list <- function(search_dirs) {
    out <- character(0)
    for (directory in search_dirs) {
        if (!dir.exists(directory)) {next}
        articles <- list_files_with_exts(directory,
                                         exts = c('html', 'Rd', 'md'))
        out <- c(out, articles)
    }
    return(out)
}

#' Extract URL links from documentation files
#'
#' @param path character string of file path to extract links from
#' @inheritParams clean_links
#'
#' @details Currently extracts links for HTML, Rd, and markdown files. Allows
#'    for future addition of file types if needed
#'
#' @importFrom tools file_ext

parse_file <- function(path, base_url) {
    if (missing(base_url)) base_url <- ""
    if (file_ext(path) == "html") {
        links <- get_html_links(path, base_url = base_url)
    } else if (file_ext(path) == "Rd") {
        links <- get_rd_links(path, base_url = base_url)
    } else if (file_ext(path) == "md"){
        links <- get_md_links(path, base_url = base_url)
    } else {
        links <- character(0)
    }
    return(links)
}
