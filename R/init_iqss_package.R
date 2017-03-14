#' Initialize a new R package skeleton using IQSS best practices
#'
#' @param path required character string of the location to create new package.
#'   The last component of the path will be used as the package name.
#' @param author_name required character string listing the primary package
#'   author's first and last name. Please also include an email address.
#'   Should be in the format `"'Yoni Ben-Meshulam' <yoni@opower.com>"`
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
#' init_iqss_package(path = 'mypkg',
#'                   author_name = "'Yoni Ben-Meshulam' <yoni@opower.com>"
#' }
#'
#' @importFrom devtools create use_readme_rmd github_pat use_cran_badge use_news_md use_gpl3_license use_github use_git use_testthat use_travis use_appveyor
#' @importFrom pkgdown build_site
#' @md
#' @export

init_iqss_package <- function(path,
                              author_name,
                              description = getOption("devtools.desc"),
                              use_rstudio = TRUE,
                              github_auth_token = github_pat(),
                              ...)
{
    if (missing(author_name) || !is.character(author_name))
        stop("author_name is required.\nPlease use the format: 'Yoni Ben-Meshulam' <yoni@opower.com>")
    else
        if (missing(description)) description <- list()
        description$Maintainer = author_name

    # init bare package --------------------------------------------------------
    pkg_name <- basename(normalizePath(path, mustWork = FALSE))
    message(sprintf('Initializing %s package . . .\n', pkg_name))
    devtools::create(path = path, description = description, rstudio = use_rstudio)

    ## Set package as working directory ----------------------------------------
    message(sprintf('Changing working directory to: %s.\n', path))
    setwd(path)
    ## Include dynamic documentation -------------------------------------------
    message('Initializing components . . .\n')

    ## RMarkdown based README
    message('---- dynamic documentation . . .\n')
    use_readme_rmd()
    use_cran_badge()

    ## NEWS
    use_news_md()

    ## Website
    pkgdown::build_site()

    # LICENSE
    message('---- GPL-3 License <https://www.gnu.org/licenses/gpl-3.0.en.html>\n')
    use_gpl3_license()

    # Version Control ----------------------------------------------------------
    message('---- version control\n')
    if (!missing(github_auth_token) || is.null(github_auth_token)) {

        use_github(auth_token = github_auth_token, ...)
    }
    else {
        message("Initializing local git repo.\n  Please use a remote git service such as GitHub (<https://help.github.com/articles/adding-a-remote/>) to store and collaborate on your package's source code.")
        use_git()
    }

    # Include testing infrastructure -------------------------------------------
    ## testthat
    message('---- testing infrastucture:\n')
    use_testthat()

    ## continuous CI
    message('           Travis CI (for Linux/MacOS testing)\n')
    use_travis()
    message('           Appveyor (for Windows testing)\n')
    use_appveyor()

    # Finalise -----------------------------------------------------------------
    message(sprintf('%s package skeleton has been build.\nPlease see [GET] for next steps.',
        pkg_name))
}
