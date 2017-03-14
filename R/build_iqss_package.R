#' Build an IQSS Best Practices compliant package, including dynamically
#' generating documentation and running tests
#'
#' @param path character string of the package path
#'
#'
#'
#' @importFrom rmarkdown render
#' @export

build_iqss_package <- function(path = '.') {


    message('knitting README.Rmd')
    render('README.Rmd')
}
