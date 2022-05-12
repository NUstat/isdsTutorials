
#' Tutorial HTML dependency
#'
#' @details HTML dependency for core tutorial JS and CSS. This should be included as a
#' dependency for custom tutorial formats that wish to ensure that that
#' tutorial.js and tutorial.css are loaded prior their own scripts and stylesheets.
#'
#' @export
ISDS_html_dependency <- function() {
  htmltools::htmlDependency(
    name = "tutorial",
    version = utils::packageVersion("ISDStutorials"),
    src = html_dependency_src("lib", "tutorial"),
    script = list(src = "print-format.js"),
    stylesheet = list(src = "print-format.css"),
    all_files = TRUE
  )
}
