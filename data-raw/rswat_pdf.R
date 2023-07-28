## Dean Koch, 2022
## prepares the `rswat_io_pdf` dataset

library(here)
library(pdftools)

# all the work is done by helper functions defined in this file
source(here('data-raw/rswat_pdf_helpers.R'))

# path to the PDF on my local machine
pdf_path = 'D:/rswat/data-raw/inputs_swatplus_rev60_5.pdf'

# update: summer 2023, I can no longer find this file on the SWAT+ website
# or any mention of it on google. Following the web based documentation leads
# to link to the SWAT+ source code repo but I can find no input documentation
# there either. So for now we use our local copy of the old file

# run the workflow
rswat_io_pdf = rswat_open_io(pdf_path)

# rswat_io_pdf is a data frame with variable definition scraped from PDF
str(rswat_io_pdf)

# update the package data
usethis::use_data(rswat_io_pdf, internal=TRUE, overwrite=TRUE)
