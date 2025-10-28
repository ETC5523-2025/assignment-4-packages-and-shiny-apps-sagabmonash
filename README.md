
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neonWaterQuality

[![Pkgdown](https://img.shields.io/badge/pkgdown-website-blue)](https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-sagabmonash/)

Welcome to the **neonWaterQuality** R package, this package provides
high-frequency water quality data from three NEON sites (ARIK (Arikaree
River), CARI (Caribou Creek), LEWI(Lewis Run)) for 2018-2019, as
analyzed in Kermorvant et al. (2023). It also includes a Shiny
application to explore the data interactively.

This package was created for Assessment 4 of ETC5523 (Communicating with
Data).

## Installation

You can install the development version of neonWaterQuality from
[GitHub](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-sagabmonash)
with:

``` r
# install.packages("remotes")
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-sagabmonash")
```

## Quick Start

To explore the data, you can launch the interactive Shiny app:

``` r
library(neonWaterQuality)
launch_app()
```

## Website

The full documentation and package website is available at:
<https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-sagabmonash/>
