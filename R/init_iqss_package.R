#' Initialize a new R package skeleton using IQSS best practices
#'
#' @param path character string of the location to create new package.
#'   The last component of the path will be used as the package name.
#' @param description list of DESCRIPTION values to override default values or
#'   add additional values.
#' @param use_rstudio logical whether to create an Rstudio project file
#'   <https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects>
#'   (with \code{\link{use_rstudio}})?
#' @param use_pkgdown logical whether or not to initialize a pkgdown website
#'   for documenting the package.
#' @param use_gpl3 logical whether or not to include a GPL 3 license.
#' @param use_tests logical whether or not to initialize a test suite with the
#'   testthat package and continuous integration with Travis CI (for Linux and
#'   macOS) and Appveyor (for Windows).
#' @param use_git logical whether or not to use git version control.
#' @param github_auth_token Provide a personal access token (PAT) from
#'  <https://github.com/settings/tokens>. Defaults to the `GITHUB_PAT`
#'  environment variable. If `NULL` then only a local git repo is initialized,
#'  not a remote GitHub repo.
#' @param github_protocol character string specifying the transfer protocol for pushing
#'  the GitHub remote repository , either `"ssh"` (the default) or `"https"`.
#'  Only relevant if `github_auth_token` is provided.
#' @param change_wd logical whether to change to make the package the working
#'   directory upon completion.
#' @param ... arguments to pass to methods.
#'
#' @examples
#' \dontrun{
#' # Initialize new package called "mypkg" in the current working directory
#' init_iqss_package(path = 'mypkg',
#'                   description = list("Title" = "A Test IQSS Package",
#'                                      "Description" =
#'                                      "This is a longer description of the package."))
#' }
#'
#' @seealso \code{\link{create}} \code{\link{github_pat}}
#' \code{\link{use_news_md}}
#' \code{\link{use_gpl3_license}} \code{\link{use_github}}
#' \code{\link{use_git}} \code{\link{use_testthat}} \code{\link{use_travis}}
#' \code{\link{use_appveyor}} \code{\link{use_package_doc}},
#' \code{\link{init_site}}
#' @importFrom devtools create github_pat use_news_md
#' use_github use_git use_testthat use_travis use_appveyor
#' use_package_doc
#' @importFrom git2r repository commit push
#' @importFrom pkgdown init_site
#' @importFrom usethis use_gpl3_license
#' @md
#' @export

init_iqss_package <- function(path,
                              description = getOption("devtools.desc"),
                              use_rstudio = TRUE,
                              use_pkgdown = TRUE,
                              use_gpl3 = TRUE,
                              use_tests = TRUE,
                              use_git = TRUE,
                              github_auth_token = github_pat(),
                              github_protocol = "https",
                              change_wd = TRUE,
                              ...)
{
    if (missing(path))
        stop('path is required to initialize a new package.', call. = FALSE)

    old_wd <- getwd()
    # init bare package --------------------------------------------------------
    pkg_name <- basename(normalizePath(path, mustWork = FALSE))
    message(sprintf('\nInitializing %s package . . .\n', pkg_name))
    devtools::create(path = path, description = description,
                     rstudio = use_rstudio)

    ## Set package as working directory ----------------------------------------
    message(sprintf('\nChanging working directory to: %s.\n', path))
    setwd(path)

    # Version Control ----------------------------------------------------------
    if (use_git) {
        message('\n\n---- Version Control ----\n')
        
        use_github <- !missing(github_auth_token) || !is.null(github_auth_token)
        
        if (use_github) {
            use_github(auth_token = github_auth_token,
                        protocol = github_protocol, ...)
        }
        else {
            message("Initializing local git repo.\n\nPlease use a remote git service such as GitHub (<https://help.github.com/articles/adding-a-remote/>) to store and collaborate on your package's source code.\n")
            use_git()
        }
    }
    else message('\n\n---- No version control initialized. Please reconsider.\n')


    ## Include dynamic documentation -------------------------------------------

    ## RMarkdown based README
    message('\n\n---- Dynamic Documentation ----\n')
    iqss_use_readme_rmd()

    cat('\n')

    # Roxygen template
    use_package_doc()

    cat('\n')

    ## NEWS
    use_news_md()

    cat('\n')

    ## Website
    if (use_pkgdown) {
        message('Creating website skeleton')
        init_site()
    }
    else warning('\n*** No package website initialized ***\n', call. = FALSE)

    # LICENSE
    if (use_gpl3) {
        message('\n\n---- GPL-3 License <https://www.gnu.org/licenses/gpl-3.0.en.html> ----\n')
        use_gpl3_license()
    }
    else warning('\n*** No LICENSE included ***\n', call. = FALSE)

    # Include testing infrastructure -------------------------------------------
    if (use_tests) {
        message('\n\n---- Testing Infrastucture ----\n')
        add_test_suite(...)
    }
    else warning('\n*** No test suite initialized ***\n', call. = FALSE)


    # Finalise -----------------------------------------------------------------
    message('\n\n---- Finalizing init ----\n')

    if (use_git) {
        commit_msg <- c('documentation')
        if (use_gpl3) commit_msg <- paste(commit_msg, 'license', sep = ', ')
        if (use_tests) commit_msg <- paste(commit_msg, 'tests', sep = ', ')
        
        add_commit_push(commit_msg = commit_msg, use_github = use_github)
    }
    if (change_wd)
        message(sprintf('\nChanging working directory to: %s.\n', path))
    else
        setwd(old_wd)

        message(sprintf(
                '%s package skeleton has been initialized.\nFor next steps see:\n  https://github.com/IQSS/social_science_software_toolkit/blob/master/best_practices_per_language/r_quickstart_guide.md',
                pkg_name))
}
