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
