# testing integration of the three big pieces: rswat, rswat.uyr, WxArchive

library(rswat.uyr)
library(devtools)
#install()

# TODO: add a check to rswat() to see if docs is loaded yet and if not, load it


#load_all()
library(rswat)

# x ='D:/rswat_data/yellowstone/split/lamar_river/qswat/lamar_river/Scenarios/Default/TxtInOut'
# rswat(x, include='basic')
# rswat_open('pcp1.pcp')



# TODO: check behavior of NWIS update calls on individual sub-catchments

# pick one of the paths to the QSWAT projects
data_dir = 'D:/rswat_data/yellowstone'
sub_dirs = save_split(data_dir)[['sub']]
i = 3

# extract the SWAT+ path
sub_dir = sub_dirs[i]
qswat_path = run_qswat(sub_dir)[['output']] |> readLines() |> jsonlite::fromJSON()
swat_dir = qswat_path[['txt']]

# find simulator path and select latest version in SWAT+ Editor assets
swat_exe = qswat_path[['simulator_dir']] |>
  list.files('^rev.+rel\\.exe$', recursive=TRUE, full.names=TRUE) |>
  sort() |> tail(1)

# load project and assign simulator
swat_dir |> rswat(swat_exe, include='basic')

# fix the default print settings to show warm-up
rswat_open('print.prt')[[1]] |> dplyr::mutate(nyskip=0L) |> rswat_write(overwrite=TRUE)

# summary now shows the correct dates
#rswat()
# good example here of type checking warning user about `0` instead of `0L` or `as.integer(0)`


# open a weather file
rswat_files(include='weather')
pcp = rswat_open('pcp1.pcp')
pcp


sub_dirs = save_subbasins(data_dir)


pcp[[1]]$nbyr=10L
new_df = pcp[[1]]
new_df |> rswat_write(overwrite=T, quiet=T)

#
rswat_dir() |> file.path('pcp1.pcp') |> readLines()
# it works!


# TODO: find the mapping of polygon IDs to subbasins
# TODO: then copy weather data in a loop
# TODO: run simulation and identify outlet object ID

# TODO: select parameters, select bounds
# TODO: model fitter - load gage data, set time









