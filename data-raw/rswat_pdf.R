## Dean Koch, 2022
## prepares the internal `.rswat_io_pdf` dataset

library(here)
library(pdftools)

# all the work is done by helper functions defined in this file
source(here('data-raw/rswat_pdf_helpers.R'))

# path to the PDF on my local machine
pdf_path = 'D:/UYRW_data/development/inputs_swatplus_rev60_5.pdf'

# run the workflow
.rswat_io_pdf = rswat_open_io(pdf_path)

# .rswat_io_pdf is a data frame with variable definition scraped from PDF
str(.rswat_io_pdf)

# update the package sysdata.rda
usethis::use_data(.rswat_io_pdf, internal=TRUE, overwrite=TRUE)
