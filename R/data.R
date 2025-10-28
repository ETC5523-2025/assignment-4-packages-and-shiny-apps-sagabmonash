#' High-Frequency Water Quality Data from NEON
#'
#' A dataset containing 15-minute aggregated water quality measurements from three NEON sites: Arikaree River, Caribou Creek, and Lewis Run, spanning 2018-2019. This data is processed from the raw data provided by NEON, following the methods in Kermorvant et al. (2023).
#'
#' @format A data frame with {42315} rows and {10} variables:
#' \describe{
#'   \item{siteID}{NEON site code (ARIK, CARI, LEWI)}
#'   \item{siteName}{Full name of the NEON site}
#'   \item{datetime}{Timestamp for the 15-minute interval (POSIXct)}
#'   \item{nitrate}{Nitrate concentration (µmol/L)}
#'   \item{temperature}{Water temperature (°C)}
#'   \item{dissolved_oxygen}{Dissolved oxygen (mg/L)}
#'   \item{specific_conductance}{Specific conductance (µS/cm)}
#'   \item{turbidity}{Turbidity (FNU)}
#'   \item{log_turbidity}{Log-transformed turbidity (log(turbidity + 1))}
#'   \item{elevation}{Surface water elevation (meters)}
#' }
#' @source \url{https://doi.org/10.1371/journal.pone.0287640}
#' @source \url{https://data.neonscience.org}
"neon_water_quality"