% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{iqss_use_readme_rmd}
\alias{iqss_use_readme_rmd}
\title{Create README files.}
\source{
Modified from usethis
}
\usage{
iqss_use_readme_rmd(base_path = ".")
}
\arguments{
\item{base_path}{Path to package root.}
}
\description{
Creates skeleton README files with sections for
\itemize{
\item a high-level description of the package and its goals
\item R code to install from GitHub, if GitHub usage detected
\item a basic example
}
Use \code{Rmd} if you want a rich intermingling of code and data. Use
\code{md} for a basic README. \code{README.Rmd} will be automatically
added to \code{.Rbuildignore}. The resulting README is populated with default
YAML frontmatter and R fenced code blocks (\code{md}) or chunks (\code{Rmd}).
}
\examples{
\dontrun{
iqss_use_readme_rmd()
}
}
