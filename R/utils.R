#' Initialise test suite skeleton
#'
#' @param pkg package description, can be path or package name. See
#'   \code{\link{as.package}} for more information
#' @param use_travis logical whether or not to include a `.travis.yml` file
#'   for using Travis CI.
#' @param use_appveyor logical whether or not to include a `appveyor.yml` file
#'   for using Appveyor CI.
#' @param ... arguments to pass to methods
#'
#' @examples
#' \dontrun{
#' add_test_suite(pkg = 'mypackage')
#' }
#'
#' @importFrom devtools use_testthat use_travis use_appveyor
#' @export

add_test_suite <- function(pkg = ".", use_travis = TRUE,
                            use_appveyor = TRUE, ...) {
    # testing suite
    use_testthat(pkg = pkg, ...)

    cat('\n')

    ## continuous CI
    message('\n           ---- Travis CI (for Linux/MacOS testing) ----\n')
    suppressMessages(use_travis(pkg = pkg, ...))
    message("* Creating `.travis.yml` from template.\n* Adding `.travis.yml` to `.Rbuildignore`.\nNext:\n * Turn on travis for this repo at https://travis-ci.org/profile\n")
    message('           ---- Appveyor (for Windows testing) ----\n')
    suppressMessages(use_appveyor(pkg = pkg, ...))
    message("* Creating `appveyor.yml` from template.\n* Adding `appveyor.yml` to `.Rbuildignore`.\nNext:\n * Turn on AppVeyor for this repo at https://ci.appveyor.com/projects\n")
}



#' Check if a package uses git version control
#' @param path character string of the package path.
#'
#' @importFrom git2r discover_repository
#' @source From devtools

uses_git <- function (path = ".")
{
    !is.null(git2r::discover_repository(path, ceiling = 0))
}

#' Check if a package uses github
#' @param path character string of the package path.
#'
#' @importFrom git2r repository
#' @source From devtools

uses_github <- function (path = ".")
{
    if (!uses_git(path))
        return(FALSE)
    r <- repository(path, discover = TRUE)
    r_remote_urls <- git2r::remote_url(r)
    any(grepl("github", r_remote_urls))
}

#' @source From devtools

uses_testthat <- function (pkg = ".")
{
    pkg <- as.package(pkg)
    paths <- c(file.path(pkg$path, "inst", "tests"), file.path(pkg$path,
        "tests", "testthat"))
    any(dir.exists(paths))
}

#' Create README files.
#'
#' Creates skeleton README files with sections for
#' \itemize{
#' \item a high-level description of the package and its goals
#' \item R code to install from GitHub, if GitHub usage detected
#' \item a basic example
#' }
#' Use \code{Rmd} if you want a rich intermingling of code and data. Use
#' \code{md} for a basic README. \code{README.Rmd} will be automatically
#' added to \code{.Rbuildignore}. The resulting README is populated with default
#' YAML frontmatter and R fenced code blocks (\code{md}) or chunks (\code{Rmd}).
#'
#' @inheritParams iqss_use_template
#' @examples
#' \dontrun{
#' iqss_use_readme_rmd()
#' }
#' @importFrom usethis use_git_hook
#' @source Modified from usethis
#' @export

iqss_use_readme_rmd <- function(base_path = ".") {

    data <- package_data(base_path)
    data$Rmd <- TRUE

    iqss_use_template(
                    "omni-README",
                    "README.Rmd",
                    data = data,
                    ignore = TRUE,
                    open = TRUE,
                    base_path = base_path
    )
    usethis_use_build_ignore("^README-.*\\.png$", escape = FALSE,
                              base_path = base_path)

    if (uses_git(base_path) && !file.exists(base_path, ".git", "hooks",
        "pre-commit"))
    {
        usethis::use_git_hook(
                            "pre-commit",
                            render_template("readme-rmd-pre-commit.sh"),
                            base_path = base_path
        )
    }
  invisible(TRUE)
}

#' @source From usethis

usethis_use_build_ignore <- function(files, escape = TRUE, base_path = ".")
{
    if (escape) {
        files <- escape_path(files)
    }
    path <- file.path(base_path, ".Rbuildignore")
    write_union(path, files)
    invisible(TRUE)
}

#' @source From usethis

escape_path <- function(x)
{
    x <- gsub("\\.", "\\\\.", x)
    x <- gsub("/$", "", x)
    paste0("^", x, "$")
}

#' @importFrom rstudioapi isAvailable hasFun navigateToFile
#' @source From usethis

open_in_rstudio <- function(path, base_path = ".") {
    path <- file.path(base_path, path)

    if (!rstudioapi::isAvailable())
        return()
    if (!rstudioapi::hasFun("navigateToFile"))
        return()
    rstudioapi::navigateToFile(path)
}

#' @source From usethis

write_union <- function(path, new_lines, quiet = FALSE)
{
    stopifnot(is.character(new_lines))
    if (file.exists(path)) {
        lines <- readLines(path, warn = FALSE)
    }
    else {
        lines <- character()
    }
    new <- setdiff(new_lines, lines)
    if (!quiet && length(new) > 0) {
        quoted <- paste0("'", new, "'", collapse = ", ")
        message("* Adding ", quoted, " to '", basename(path),
            "'")
    }
    all <- union(lines, new_lines)
    writeLines(all, path)
}

#' Add, commit, and push a repository
#'
#' @param path A path to an existing local git repository.
#' @param commit_msg chracter string commit message.
#' @param use_github logical whether or not to push the commit to GitHub.
#'
#' @importFrom git2r add branches repository commit push

add_commit_push <- function(path = '.', commit_msg, use_github = TRUE) {
    repo <- repository(path)
    git2r::add(repo = repo, path ='.')
    commit(repo = repo, message = commit_msg, all = TRUE)
    if (use_github) {
        remote_branch <- try(branches(repo)[[2]], silent = TRUE)
        if (!("try-error" %in% attr(remote_branch, "class"))) {
            push_result <- try(push(object = repo), silent = TRUE)
            if ("try-error" %in% attr(push_result, "class")) {
                message(sprintf("Unable to push %s to GitHub.", commit_msg))
                error_msg <- "Unable to authenticate with supplied credentials"
                if (any(grepl(error_msg, attr(push_result, "condition"))))
                    message('  To allow IQSSdevtools to push to your repository, you may need an SSH key added to the ssh-agent.\n  See: https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/')
            }
        }
    }
}

test_doc_links <- function(path = ".", base_url) {
    ## Assumes that the current working directory is Zelig package top directory
    library(xml2)
    library(rvest)
    library(httr)

    ## Allow code to handle relative paths handed to it
    if (path == ".") {
        path <- getwd()
    } else {
        setwd(path)
        path <- getwd()
    }

    bad_articles <- "" ## Hacky declare to allow for later append
    bad_links <- ""
    current_dir <- ""
    if (devtoolsis.package()) {
        search_dirs <- c("man","docs","docs/articles","docs/news","docs/reference")
    } else {
        search_dirs <- path
    }
    for (directory in search_dirs) {
      articles <- list.files(pattern = "html")
      for (article in articles) {
          links <- get_links(article)
          for (link in links) {
              if (bad_link(link)) {
                  current_dir <- c(current_dir, directory)
                  bad_articles <- c(bad_articles, article)
                  bad_links <- c(bad_links, link)
              }
          }
      }
      setwd(path)
    }

    ## Clean Up Code
    current_dir <- current_dir[2:length(current_dir)]
    bad_articles <- bad_articles[2:length(bad_articles)] ## Clean up earlier hack
    bad_links <- bad_links[2:length(bad_links)]


    return(data.frame(current_dir,bad_articles,bad_links))



}

#' Parses an html document and returns a vector of all links in the document
#'
#' @param html_doc A path to an html file
#' @author Ben Sabath
#' @return Vector of urls
#'
get_html_links <- function(html_doc) {
    doc <- read_html(html_doc)
    links <- html_attr(html_nodes(doc, "a"), "href")
    links <- clean_links(links)
    return(links)
}


#' Code to handle finding links in non-html documents.
#' Currently cannot handle relative links
#'
#'
#'
#'
#'
get_other_links <- function(rd_doc) {
    links <- ""
    doc_str <- readLines(rd_doc)
    words <- strsplit(doc_str,"[{} ()]")
    for (line in words) {
        for (word in line){
            if (grepl("^http.*//",word)) {
                links <- c(links, word)
            }
        }
    }
    links <- links[2:length(links)]
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


#' A function for readability that handles checking of whether or not a link is functional
#'
#'
#' @param link a URL
#' @author Ben Sabath
#' @return boolean of whether or not the link is functional
bad_link <- function(link) {
    pg <- GET(link)
    return(pg$status != 200)
}
