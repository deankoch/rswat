## prepares the internal `.rswat_docs` dataset

library(here)

# all the work is done by helper functions defined in this file
source(here('data-raw/rswat_docs_helpers.R'))

# path to the PDF on my local machine
pdf_path = 'D:/UYRW_data/development/inputs_swatplus_rev60_5.pdf'

# run the workflow
.rswat_io_pdf = rswat_open_io(pdf_path)
str(.rswat_io_pdf)

# update the package sysdata.rda
usethis::use_data(.rswat_io_pdf, overwrite=TRUE)
