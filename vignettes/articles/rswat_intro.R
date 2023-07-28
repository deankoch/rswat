# Helper script for the rswat_intro.Rmd article

library(devtools)
load_all()
library(rswat.maker)
# rmarkdown::render('D:/rswat/vignettes/articles/rswat_intro.Rmd')

# wipe existing output directory
dest_dir = 'D:/rswat_data/swat/soda_test'
if( dir.exists(dest_dir) ) unlink(dest_dir, recursive=TRUE)
dir.create(dest_dir, recursive=TRUE)


lamar_dir = 'D:/rswat_data/lamar'
soda_dir = save_split(lamar_dir)[['sub']][2]
soda_qswat = run_qswat(soda_dir)[['output']] |> readLines() |> jsonlite::fromJSON()

# plot basin and overlay sub-basins found with SWAT
sub_dir = plot_qswat(soda_dir, quiet=TRUE, main='Soda Butte headwater (QSWAT+)')

# get paths required below
txt_dir = soda_qswat[['txt']]
swat_path = soda_qswat[['simulator_dir']] |>
  file.path('SWATPlusEditor/resources/app.asar.unpacked/static/swat_exe/rev60.5.7_64rel.exe')


# the source and destination directory names
basename(txt_dir)
basename(dest_dir)

# make the copy
txt_dir |> rswat_restore(dest_dir, overwrite=TRUE) |> head()

rswat(swat_dir=dest_dir, quiet=TRUE, exe_path=swat_path)


# rswat()
#
# rswat_files()
#
#
# rswat_files('climate')
#
#
# rswat_find(pattern='IPET', fuzzy=2)
#
# rswat_find(pattern='codes.bsn')
#
# rswat_docs('codes.bsn')
#
# rswat_docs('IPET')
#
#
# rswat_open('time.sim') |> str()
#
# rswat_open('print.prt') |> head(2)




rswat()

rswat_open('basin_wb_yr.txt') |> rswat_date_conversion() |> dplyr::tibble()


rswat_sim_dates(lazy = FALSE,
                prt = TRUE,
                render = TRUE,
                .db = .rswat_db)


prt = rswat_open('print.prt')[[1]]
prt$nyskip = 0L
prt |> rswat_write(overwrite=TRUE)

rswat_exec()
rswat_open('basin_wb_yr.txt')[, seq(8)] |> rswat_date_conversion()




rswat_exec()

rswat_open('basin_wb_yr.txt') |> dplyr::tibble()



sim = rswat_open('time.sim')
prt = rswat_open('print.prt')[[1]]

# assign new values
nm_copy = c('day_start', 'yrc_start', 'day_end', 'yrc_end')
sim['yrc_start'] = sim['yrc_start'] - 1L
prt[nm_copy] = sim[nm_copy]

sim |> rswat_write(overwrite=TRUE)
prt |> rswat_write(overwrite=TRUE)
