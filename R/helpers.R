#' Use a IQSS template
#'
#' @param template Template name
#' @param save_as Name of file to create. Defaults to \code{save_as}
#' @param data A list of data passed to the template.
#' @param ignore Should the newly created file be added to \code{.Rbuildignore?}
#' @param open Should the new created file be opened in RStudio?
#' @param base_path Path to package root.
#'
#' @source Modified from usethis.
#' @keywords internal

iqss_use_template <- function(template,
                         save_as = template,
                         data = list(),
                         ignore = FALSE,
                         open = FALSE,
                         base_path = "."
                         ) {

    template_contents <- render_template(template, data)
    write_over(template_contents, file.path(base_path, save_as))

    if (ignore) {
        usethis_use_build_ignore(save_as, base_path = base_path)
  }

    if (open) {
        message("* Modify '", save_as, "'")
        open_in_rstudio(save_as, base_path = base_path)
    }
    invisible(TRUE)
}


#' Render IQSS template
#'
#' @param template character string identifying which template to render
#' @param data list with data to render in the template
#' 
#' @source Modified from usethis.
#' @importFrom whisker whisker.render

render_template <- function(template, data = list()) {
    template_path <- find_template(template)
    paste0(whisker.render(readLines(template_path), data), "\n",
            collapse = "")
}

#' @source Modified from usethis.

find_template <- function(template_name) {
    path <- system.file("templates", template_name, package = "IQSSdevtools")
    if (identical(path, "")) {
        stop("Could not find template '", template_name, "'", call. = FALSE)
    }
    path
}


#' @source Modified from usethis.

write_over <- function(contents, path) {
  stopifnot(is.character(contents), length(contents) == 1)

  if (same_contents(path, contents))
    return(FALSE)

  if (!can_overwrite(path))
    stop("'", path, "' already exists.", call. = FALSE)

  message("* Writing '", path, "'")
  cat(contents, file = path)
  TRUE
}

#' @importFrom digest digest
#' @source Modified from usethis.

same_contents <- function(path, contents) {
  if (!file.exists(path))
    return(FALSE)

  text_hash <- digest::digest(contents, serialize = FALSE)
  file_hash <- digest::digest(file = path)

  identical(text_hash, file_hash)
}

#' @source Modified from usethis.

can_overwrite <- function(path) {
    name <- basename(path)

    if (!file.exists(path)) {
        TRUE
    } else if (interactive() && !yesno("Overwrite `", name, "`?")) {
        TRUE
    } else {
        FALSE
    }
}

#' @source Modified from usethis.

yesno <- function(...) {
    yeses <- c("Yes", "Definitely")
    nos <- c("No way", "No")

    cat(paste0(..., collapse = ""))
    qs <- c(sample(yeses, 1), sample(nos, 2))
    rand <- sample(length(qs))

    utils::menu(qs[rand]) != which(rand == 1)
}

#' @importFrom desc description
#' @importFrom gh gh_tree_remote
#' @source Modified from usethis.

package_data <- function(base_path = ".") {
    desc <- desc::description$new(base_path)

    out <- as.list(desc$get(desc$fields()))
    if (uses_github(base_path)) {
        out$github <- gh::gh_tree_remote(base_path)
    }
    out
}
