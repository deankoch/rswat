# Helper script for the rswat_lamar.Rmd article

# TODO: when finished run this again without devtools
library(devtools)
load_all()
#library(rswat)
library(rswat.maker)

# At the end of the script we build the article
markdown_file = 'D:/rswat/vignettes/articles/rswat_intro.Rmd'

# copy paths from the example QSWAT+ project
soda_dir = 'D:/rswat_data/lamar_contemporary/split/soda_butte_cr_nr_lamar_ranger_station_ynp'
soda_qswat = rswat.maker::run_qswat(soda_dir)[['output']] |> readLines() |> jsonlite::fromJSON()
soda_txt_dir = soda_qswat[['txt']]

# wipe existing output directory
output_dir = 'D:/rswat_data/swat/soda_test'
if( dir.exists(output_dir) ) unlink(output_dir, recursive=TRUE)

# copy TxtInOut directory to test directory
txt_dir |> rswat_restore(output_dir, overwrite=TRUE)






#

# when finished, render the article
#rmarkdown::render(markdown_file)

