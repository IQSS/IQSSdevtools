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
    use_travis(pkg = pkg, ...)
    message('           ---- Appveyor (for Windows testing) ----\n')
    use_appveyor(pkg = pkg, ...)
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
