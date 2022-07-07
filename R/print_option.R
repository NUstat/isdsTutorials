#' Tutorial print option
#'
#' @description
#' Print the tutorial. Should load print-file.css into tutorial yaml
#' for nice looking document.
#' @param label Label to appear on the print button
#'
#' @export
#print grade
print_ui <- function(label = "print tutorial") {
  #Add css print formatting
  includeCSS(system.file("www/print-format.css", package = "ISDStutorials"))

  #Print JS uses CSS formatting
  jscode <- HTML("$(document).on('shiny:inputchanged', function(event){ if (event.name === 'printButton') {window.print();}});")

  shinyjs::useShinyjs()
  shinyjs::extendShinyjs(text = jscode,functions = c())
  actionButton("printButton", label = label, style="opacity: .7; color: #000;")

}
