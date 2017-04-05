#' Build an IQSS Best Practices compliant package, including dynamically
#' generating documentation and running tests
#'
#' @param path character string of the package path.
#' @param rdocs logical whether or not to \code{roxegenize} the R documentation
#'   and vignettes.
#' @param rdocs_args character vector of roclet names to use with package. This
#'   defaults to `NULL`, which will use the `roclets` fields in the list
#'   provided in the Roxygen `DESCRIPTION` field. If none are specified,
#'   defaults to `c("collate", "namespace", "rd")`.
#' @param readme logical whether or not to render the READEM.Rmd.
#' @param website logical whether or not to render the package website.
#' @param check logical whether or not to check that the package can build and
#'   passes all tests.
#' @param check_args character vector of additional arguments to pass to
#'    \code{\link{check}}.
#' @param ... arguments to pass to methods.
#'
#'
#' @seealso \code{\link{document}}. \code{\link{render}},
#'   \code{\link{build_site}}
#' @importFrom devtools as.package check document
#' @importFrom rmarkdown render
#' @importFrom pkgdown build_site
#' @export

build_iqss_package <- function(path = '.',
                               rdocs = TRUE,
                               rdocs_args = c('rd', 'collate', 'namespace'),
                               readme = TRUE,
                               website = TRUE,
                               check = TRUE,
                               check_args = '--as-cran',
                               ...) {
    pkg <- as.package(path)
    message(sprintf('Building %s . . .', pkg$package))

    setwd(path)

    root_files <- list.files()

    if (rdocs) {
        message('---- Roxegenizing the R docs and vignettes ----\n')
        document(roclets = rdocs_args)
    }

    if (readme) {
        message('---- knitting README.Rmd ----\n')
        render('README.Rmd')
    }

    if (website) {
        message('\n---- Compiling website----\n')
        if (!('_pkgdown.yml' %in% root_files))
            warning('Necessary pkgdown files needed to build package website were not found.\nWebsit not built.',
            call. = FALSE)
        else
            build_site()
    }

    if (check) {
        check(args = check_args)
    }
}
