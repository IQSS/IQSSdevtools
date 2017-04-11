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
#' @param check logical whether or not to use \code{\link{check_best_practices}}
#'   to check that the package can be built and passes all tests. Also generates
#'   an IQSS Report Card <https://github.com/IQSS/social_science_software_toolkit/blob/master/report_card/iqss_report_card_spec.md>.
#' @param git_commit logical whether or not to commit the build to git.
#' @param commit_message character string with a descriptive git commit message.
#'   Only relevant if `git_commit = TRUE`.
#' @param push_to_github logical whether or not to push the commits to GitHub.
#'   Only relevant if `git_commit = TRUE`.
#' @param ... arguments to pass to methods.
#'
#'
#' @seealso \code{\link{document}}. \code{\link{render}},
#'   \code{\link{build_site}}, \code{\link{check_best_practices}}
#' @importFrom devtools as.package check document
#' @importFrom git2r commit push repository
#' @importFrom rmarkdown render
#' @importFrom pkgdown build_site
#' @export

build_iqss_package <- function(path = '.',
                               rdocs = TRUE,
                               rdocs_args = c('rd', 'collate', 'namespace'),
                               readme = TRUE,
                               website = TRUE,
                               check = TRUE,
                               git_commit = TRUE,
                               commit_message = "IQSS package build",
                               push_to_github = TRUE,
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
        message('\n---- Compiling website ----\n')
        if (!('_pkgdown.yml' %in% root_files))
            warning('Necessary pkgdown files needed to build package website were not found.\nWebsit not built.',
            call. = FALSE)
        else
            build_site()
    }

    if (check) {
        message('\n---- Checking the package ----\n')
        check_best_practices(path = path, ...)
    }

    if (commit) {
        message('\n---- Committing the changes ----\n')
        repo <- repository(path = path)
        git2r::add(repo = repo, path = path)
        commit(repo = repo, message = commit_message, all = TRUE, ...)
        if (push_to_github)
            message('\n---- Pushing commit to GitHub ----\n')
            push(object = repo)
        if (check)
            message('Note: the IQSS Report Card in this commit will report the previous commit SHA.')

    }

    message(sprintf('%s has been built.', pkg$package))
}
