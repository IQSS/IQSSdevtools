#' Build an IQSS Best Practices compliant package, including dynamically
#' generating documentation and running tests
#'
#' @param path character string of the package path.
#' @param readme logical whether or not to render the READEM.Rmd.
#' @param website logical whether or not to render the package website.
#'
#'
#' @importFrom rmarkdown render
#' @importFrom pkgdown build_site
#' @export

build_iqss_package <- function(path = '.', readme = TRUE, website = TRUE) {

    setwd(path)

    if (readme) {
        message('---- knitting README.Rmd ----\n')
        render('README.Rmd')
    }


    if (website) {
        message('\n---- Compiling website----\n')
        build_site()
    }
}
