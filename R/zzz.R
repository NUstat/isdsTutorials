.onLoad <- function(...) {
  # shiny::addResourcePath(
  #   prefix = "assets", # custom prefix that will be used to reference your directory
  #   directoryPath = system.file("www", package = "ISDStutorials") # path to resource in your package
  # )

  htmltools::attachDependencies(
    "print",
    htmltools::htmlDependency(
      name = "print",
      version = utils::packageVersion("ISDStutorials"),
      src = system.file("www", package = "ISDStutorials"),
      script = "print-format.js",
      stylesheet = "print-format.css",
      all_files = TRUE
    )
  )
}


