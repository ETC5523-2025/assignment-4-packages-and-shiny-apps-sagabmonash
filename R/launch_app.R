#' Launch the NEON Water Quality Shiny App
#'
#' This function launches the Shiny application bundled with the package.
#'
#' @export
#' @importFrom shiny runApp
#'
#' @examples
#' # This example is not run to avoid hanging during checks
#' if (interactive()) {
#'   launch_app()
#' }
launch_app <- function() {
  appDir <- system.file("app", package = "neonWaterQuality")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing the package.")
  }
  
  # Find packages used in the app but not imported
  # This fixes the "All declared Imports should be used"
  required_packages <- c("bslib", "plotly", "DT")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg {pkg}} must be installed to run the app.",
        "i" = "You can install it with {.code install.packages('{pkg}')}"
      ))
    }
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}