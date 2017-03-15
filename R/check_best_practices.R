#' Check if a package complies with the skeleton IQSS Best Practices
#'
#' @param path character string of the package path.
#' @param save_results logical whether or not to save the results of the
#' check in the package directory in a YAML file called
#' `.iqss.bestpractices.yml`.
#' @param report_results logical whether or not to report the results to the
#' console.
#' @param list_results logical whether or not to return the results as a list.
#'
#' @examples
#' \dontrun{
#' check_best_practices()
#' }
#'
#' @importFrom covr package_coverage percent_coverage
#' @importFrom devtools as.package check use_build_ignore
#' @importFrom yaml as.yaml
#' @importFrom utils packageVersion
#' @export

check_best_practices <- function(path = ".", save_results = TRUE,
                                 report_results = TRUE, list_results = FALSE)
{
    pkg <- as.package(path)

    message(sprintf('\nSurveying %s. . .\n', pkg$package))
    pkg_files <- list.files(pkg$path, all.files = TRUE)
    bp_list <- list()

    # Check --------------------------------------------------------------------

    message('* checking documentation . . .')
    bp_list$Documentation$readme <- 'README.Rmd' %in% pkg_files

    #bp_list$roxygen <-

    bp_list$Documentation$news <- 'NEWS.md' %in% pkg_files

    bp_list$Documentation$vignettes <- 'vignettes' %in% pkg_files

    if ('docs' %in% pkg_files) {
        doc_files <- list.files(sprintf('%s/docs', pkg$path))
        bp_list$Documentation$pkgdown_website <- 'docs/index.html' %in% doc_files
    }
    else
        bp_list$Documentation$pkgdown_website <- FALSE

    bp_list$Documentation$bugreports <- 'bugreports' %in% names(pkg)

    message('* checking license . . .')
    if ('license' %in% names(pkg))
        bp_list$License$gpl3_license <- grepl('GPL-3', pkg$license)
    else
        bp_list$License$gpl3_license <- FALSE

    message('* checking version control . . .')
    bp_list$Version_Control$git <- uses_git(pkg$path)
    bp_list$Version_Control$github <- uses_github(pkg$path)

    message('* checking tests . . .')
    bp_list$Testing$uses_testthat <- uses_testthat(pkg$path) &
                                                    'tests' %in% pkg_files
    bp_list$Testing$uses_travis <- '.travis.yml' %in% pkg_files
    bp_list$Testing$uses_appveyor <- 'appveyor.yml' %in% pkg_files
    message(' ---- running CRAN check ----')
    cran_check_results <- suppressMessages(check(pkg$path, quiet = TRUE))
    bp_list$Testing$no_check_warnings <- length(cran_check_results$warnings) == 0
    bp_list$Testing$no_check_errors <- length(cran_check_results$errors) == 0
    bp_list$Testing$no_check_notes <- length(cran_check_results$notes) == 0

    message(' ---- calculating test coverage ----')
    bp_list$Testing$test_coverage <- percent_coverage(
                                        package_coverage(pkg$path))

    bp_list$Background$package_name <- pkg$package
    bp_list$Background$package_version <- pkg$version
    bp_list$Background$check_time <- as.character(Sys.time())
    # Results ------------------------------------------------------------------
    bp_yml <- as.yaml(bp_list)
    if(report_results) {
        cat(sprintf('\nSurvey results for %s:\n---------------------------------------\n',
            pkg$package))
        cat(bp_yml)
    }
    if (save_results) {
        yml_name <- '.iqss_reportcard.yml'
        yml_path <- sprintf('%s/%s', pkg$path, yml_name)
        message(sprintf('\nSaving results into %s', yml_name))

        if (yml_name %in% pkg_files) file.remove(yml_name)
        iqssdevtools_version <- packageVersion("IQSSdevtools")
        create_msg <- sprintf('# Created by IQSSdevtools (%s). Do not edit by hand.',
                              iqssdevtools_version)
        cat(create_msg, '\n\n', bp_yml, file = yml_path)
        use_build_ignore(yml_name)
        message(sprintf('* Adding %s to .Rbuildignore', yml_name))
    }
    if (list_results) return(bp_list)
}
