
<!-- README.md is generated from README.Rmd. Please edit that file -->
Ecosystem Context for Stock Advice (ECSA)
=========================================

The goal of the ECSA project is to create bespoke documents containing contextual ecosystem information for managed stocks on the Northeast Continental Shelf. The document creation process relies on the use of survey strata (as defined by the NEFSC [Bottom Trawl Survey](https://www.nefsc.noaa.gov/femad/ecosurvey/mainpage/)) to identify stock areas from which time series of area-averaged ecosystem information can be drawn. For each area of interest, we have automated the aggregation of season-specific time series of ocean temperature and salinity (surface and bottom), zooplankton composition and abundance, and chlorophyll concentration. Each document contains time series of estimated area occupied by a given stock within the specified strata and within the ecosystem as a whole. We also provide estimates for minimum population size (abundance and biomass) based on yearly restratification of habitat given estimates of habitat occupancy.

Note: The ECSA is not yet in R package format. Hence, functions must be sourced from the `R/` directory during the document development process. Template `.Rmd` files source the functions by default.
