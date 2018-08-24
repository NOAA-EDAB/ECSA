### Create new templates: a proof of concept ###

## The goal is to have a generic template that we can use to generate stock specific .rmd
## files complete with plots that are ready to add bespoke information and synthesis.
## The generic template has tags for R objects (e.g., {SAMPLE_TAG}) that are filled in  

## Step 1. Update test_template.Rmd according to how you want the final document to look.
## This is the generic template that the subsequent documents will be generated from.

## Step 2. The template_control() function takes the test_template.Rmd and automatically
## converts all tags to appropriate R objects. e.g., {COMMON_NAME} == "summer flounder"
devtools::install_github("NOAA-edab/ECSA")
library(ecsa)
create_template(survdat_name = "SUMMER FLOUNDER", overwrite = FALSE)

## or, if you want to generate a series of templates, 
## lapply(c("SUMMER FLOUNDER", "ATLANTIC COD", "ATLANTIC HERRING"), create_template, overwrite = TRUE)

## Step 3. Edit the newly created "ECSA_summer-flounder.rmd and knit to html.