% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_doc_links.R
\name{get_html_links}
\alias{get_html_links}
\title{Parses an html document and returns a vector of all links in the document}
\usage{
get_html_links(html_doc, base_url = "")
}
\arguments{
\item{html_doc}{character string path to an html file}

\item{base_url}{a character string or vector of character strings for a
domain or domains for which relative links are subdomains. Used to resolve
relative URL paths in documentation.}
}
\value{
a vector of URLs
}
\description{
Parses an html document and returns a vector of all links in the document
}
\author{
Ben Sabath
}
