<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/IQSSdevtools)](https://cran.r-project.org/package=IQSSdevtools) [![Travis-CI Build Status](https://travis-ci.org/IQSS/IQSSdevtools.svg?branch=master)](https://travis-ci.org/IQSS/IQSSdevtools) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/IQSS/IQSSdevtools?branch=master&svg=true)](https://ci.appveyor.com/project/IQSS/IQSSdevtools)

IQSSdevtools
============

**This package is in the early stages of development**

The goal of **IQSSdevtools** is to makes it easy to initialize and compile an R software package that complies with the [IQSS Best Practices for Software Development and Sustainability (IBPSDS)](https://github.com/IQSS/social_science_software_toolkit/blob/master/iqss_sss_best_practices.md).

The package will contain the following functions:

-   `init_iqss_package`: initialize a new IQSS Best Practices for Software Development and Sustainability compliant package {MINIMALLY WORKING EXAMPLE}

-   `check_best_practices`: checks to what extent a package follows the minimal best practices.

-   `add_iqss_package`: identify and add (when possible) missing Best Practice compliant elements to an existing R package {IN DEVELOPMENT}

    -   `add_test_suit`: add just a test suite, including files needed for continuous integration.

-   `build_iqss_package`: builds and tests IQSS Best Practice elements in an R package {IN DEVELOPMENT}

-   `iqss_lintr`: lints package source code using IQSS R Style Guide {GUIDE and FUNCTION IN DEVELOPMENT}

-   `publish_package`: {GUIDE and FUNCTION IN DEVELOPMENT}

IQSS Best Practices for R Language Software Development and Sustainability
==========================================================================

`init_iqss_package` initializes a skeleton package that allows you to begin to implement the IQSS Best Practices for Software Development and Sustainability.

**IN DEVELOPMENT**

-   **Documentation**: Sets up documentation dynamically generated using [RMarkdown](http://rmarkdown.rstudio.com/) and [roxygen2](https://CRAN.R-project.org/package=roxygen2). This includes a *README.Rmd* file along with a package website generated with [pkgdown](https://github.com/hadley/pkgdown). The README.Rmd file should lay out core information about the package's motivation, methods and include a dynamically generated "quick-start" example. `init_iqss_package` also creates a *NEWS.md* file for you to document all changes made to the package by version.

-   **License**: Adds a [GPL-3 License](https://www.gnu.org/licenses/gpl-3.0.en.html)

-   **Version Control**: Sets up [git](https://git-scm.com/) version control.

-   **Public Source Code Hosting**: If a GitHub authentication token is provided with the `github_auth_token` argument, a new [GitHub](https://github.com/) remote repository is created for the package.

-   **Testing**: Sets up the infrastructure for [unit testing](https://en.wikipedia.org/wiki/Unit_testing) with the [testthat](https://CRAN.R-project.org/package=testthat) package. The basic foundation for external continuous integration of these unit tests is set up with the [Travis CI](https://travis-ci.org/) and [Appveyor](https://www.appveyor.com/) services to run the tests on Linux and Windows operating systems, respectively.

Install
-------

**IQSSdevtools** is in the early stages of development and is not yet on CRAN.

To install the development version using [devtools](https://CRAN.R-project.org/package=devtools):

``` r
library(devtools)

install_github('hadley/pkgdown') # Not currently on CRAN
install_github('IQSS/IQSSdevtools')
```

Example
=======

To initialize a new package called `mypkg` use:

``` r
library(IQSSdevtools)

init_iqss_package(path = 'mypkg',
                    description = list("Title" = "A Test IQSS Package")
                  )
```

To check the extent to which a package follows the best practices:

``` r
check_best_practices()
## 
## Surveying IQSSdevtools. . .
## * checking documentation . . .
## * checking license . . .
## * checking version control . . .
## * checking tests . . .
##  ---- running CRAN check ----
##  ---- calculating test coverage ----
## 
## Survey results for IQSSdevtools:
## ---------------------------------------
## Documentation:
##   readme: yes
##   roxygen: yes
##   news: yes
##   bugreports: yes
##   vignettes: yes
##   website:
##     openscholar: no
##     pkgdown_website: no
## License:
##   gpl3_license: yes
## Version_Control:
##   git: yes
##   github: yes
## Testing:
##   uses_testthat: no
##   uses_travis: yes
##   uses_appveyor: yes
##   build_check:
##     build_check_completed: yes
##     no_check_warnings: yes
##     no_check_errors: yes
##     no_check_notes: no
##   test_coverage: 0
## Background:
##   package_name: IQSSdevtools
##   package_version: 0.0.0.9000
##   package_language: R
##   package_commit_sha: dcae5a1a5d5fc6515ab978c67fc73f661060479a
##   iqss_bestpractices_version: 0.0.0.9000
##   iqssdevtools_version: 0.0.0.9000
##   check_time: 2017-04-05 09:48:08
## 
## Saving results into .iqss_reportcard.yml
## * Adding .iqss_reportcard.yml to .Rbuildignore
```

Relationship with `devtools`
============================

**IQSSdevtools** largely builds on the [devtools](https://CRAN.R-project.org/package=devtools) package. It wraps many of the **devtools** functions based on the Best Practices. As such, feel free to use any **devtools** function when developing your package to add additional elements/run specific functions.
