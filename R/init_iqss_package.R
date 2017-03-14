#' Initialize a new R package skeleton using IQSS best practices
#'
#' @param path character string of the location to create new package. The last
#'   component of the path will be used as the package name.
#' @param description list of DESCRIPTION values to override default values or
#'   add additional values.
#' @param use_rstudio logical whether to create an Rstudio project file
#'   <https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects>
#'   (with \code{\link{use_rstudio}})?
#' @param github_auth_token Provide a personal access token (PAT) from
#'  <https://github.com/settings/tokens>. Defaults to the `GITHUB_PAT`
#'  environment variable. If `NULL` then only a local git repo is initialized,
#'  not a remote GitHub repo.
#' @param ... arguments to pass to methods.
#'
#' @examples
#' \dontrun{
#' # Information to add to the DESCRIPTION file
#' description_list <- list(
#'      "Description" = "A longer packages description"
#'      "Maintainer" = "'FirstName LastName' <first.last@emial.com>")
#'
#' # Initialize package
#' init_iqss_package(path = 'mypkg', description = description_list)
#' }
#'
#' @importFrom devtools create use_readme_rmd git_pat
#' @importFrom pkgdown build_site
#' @md
#' @export

init_iqss_package <- function(path,
                              description = getOption("devtools.desc"),
                              use_rstudio = TRUE,
                              github_auth_token = git_pat(),
                              ...)
{
    # init bare package --------------------------------------------------------
    pkg_name <- basename(normalizePath(path, mustWork = FALSE))
    message(sprintf('Initializing %s package . . .\n', pkg_name))
    create(path = path, description = description, use_rstudio = use_rstudio)

    ## Set package as working directory ----------------------------------------
    message(sprintf('Changing working directory to: %s', path))
    setwd(path)
    ## Include dynamic documentation -------------------------------------------
    message('Initializing components . . .\n')

    ## RMarkdown based README
    message('   - dynamic documentation . . .\n')
    use_readme_rmd()
    use_cran_badge()

    ## NEWS
    use_news_md()

    ## Website
    message('       + package website with pkgdown\n')
    pkgdown::build_site()

    # Version Control ----------------------------------------------------------
    message('   - version control\n')
    if (!missing(github_auth_token) || is.null(github_auth_token)) {

        use_github(auth_token = github_auth_token, ...)
    }
    else {
        use_git()
    }

    }

    # Include testing infrastructure -------------------------------------------
    ## testthat
    message('   - testing infrastucture:\n')
    message('       + unit tests with testthat\n')
    use_testthat()

    ## continuous CI
    message('       + continuous integration with:\n')
    message('           Travis CI (for Linux/MacOS testing)\n')
    use_travis()
    message('           Appveyor (for Windows testing)\n')
    use_appveyor()

}
