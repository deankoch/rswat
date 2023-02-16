#'
#' Run the SWAT+ executable
#'
#' Makes a system call to run the SWAT+ executable in the currently assigned SWAT+
#' project directory (see `?rswat`). This runs the simulation specified parameterized
#' by the various config files in `swat_dir` (list them with `rswat_files`).
#'
#' Simulation outputs are printed to one or more plain text output files as
#' specified by table 4 in 'print.prt'. The function returns a list of all such files
#' modified/created by the simulator, as well as any log files written.
#'
#' @param quiet logical, suppresses console output
#' @param .db rswat reference object, for internal use
#'
#' @return a tibble of output files created
#' @export
#'
rswat_exec = function(quiet=FALSE, .db=.rswat_db) {

  # TODO: add option to make a backup of existing output files

  # get the project directory and a copy of all output files
  swat_dir = .db$get_swat_dir()
  old_output_files = rswat_files(quiet=TRUE, .db=.db)

  # get the simulator path
  exe_path = .db$get_exe_path()
  if( is.na(exe_path) ) stop('SWAT+ executable not found. Assign it with "rswat(exec_path)"')

  # build shell command with pushd to avoid changing R's working directory
  shell_prefix = paste0('pushd ', normalizePath(swat_dir, mustWork=F), ' &&')
  sys_call = paste0(shell_prefix, ' ', tools::file_path_sans_ext(normalizePath(exe_path)))

  # run the command in the windows shell (executes the SWAT simulation)
  timer_start = Sys.time()
  invisible( shell(sys_call, intern=quiet) )
  timer_end = Sys.time()

  # stop timer, prepare time elapsed message
  msg_timer = difftime(timer_end, timer_start, units='secs')[[1L]] |> round(2L) |> paste('seconds')
  if( !quiet ) message(paste('SWAT+ simulation finished in', msg_timer))

  # scan for changes in directory and return list of files modified
  .db$refresh_cio_df(quiet=quiet)
  new_output_files = rswat_files(quiet=TRUE, .db=.db)
  is_new = !( new_output_files[['file']] %in% old_output_files[['file']] )
  if( any(!is_new) )
  {
    idx_old = match(new_output_files[['file']][!is_new], old_output_files[['file']])
    is_old_modified = new_output_files[['modified']][idx_old] != old_output_files[['modified']]
    is_new[ which(!is_new)[idx_old][is_old_modified] ] = TRUE
  }

  # count how many of each type of file was written
  output_files = dplyr::arrange(new_output_files[is_new, c('file', 'type'), drop=FALSE], type)
  type_count = table(output_files[['type']])
  msg_count = paste(type_count, names(type_count), collapse=' and ') |> paste('files were written')
  if( !quiet )
  {
    # message about count then print file names and types
    message(msg_count)
    print(output_files)
  }

  return( invisible(output_files) )
}


#' Configure SWAT+ for daily output to an object hydrograph (OHG) file
#'
#' Configures the SWAT+ currently loaded SWAT+ project to product OHG output instead of
#' the output files specified in "print.prt", or (if `revert=TRUE`) vice-versa. This
#' modifies "file.cio" and creates (or deletes) "object.prt".
#'
#' OHG files contain information on a specific hydrological variable at a specific
#' location, such as discharge at the main outlet, or recharge from a particular HRU.
#' Specify an output with `i` (ID number), `obj` (object type), and `name` (variable
#' of interest). These arguments be supplied as vectors to configure multiple OHG files
#' at once.
#'
#' If `revert=TRUE`, the function ignores `i`, `obj`, and `name`, deletes the file
#' "object.prt" if it exists.
#'
#' The object type codes for `obj` are as follows:
#'
#'  HRUs:                   'hru', 'hlt', 'ru'
#'  reservoirs:             'res'
#'  channels:               'cha', 'sdc'
#'  export coefficients:    'exc'
#'  delivery ratio:         'dr'
#'  outlets:                'out'
#'
#' and the variable names for `name` are:
#'
#'  total:                  'tot'
#'  recharge:               'rhg'
#'  surface:                'sur'
#'  lateral:                'lat'
#'  tile:                   'til'
#'  soil moisture by layer: 'sol'
#'
#'
#' @param i integer, the "object number", an ID code associated with the desired object
#' @param obj character, three-character object type code (see details)
#' @param name character, one of 'tot', 'sur', 'lat', 'til', 'rhg' (object-dependent)
#' @param revert logical, removes OHG output
#' @param overwrite logical, writes changes to files on disk
#' @param quiet logical, suppresses console output
#' @param .db rswat reference object, for internal use
#'
#' @return either the data frame written to "object.prt" (revert=TRUE), or NULL (revert=TRUE)
#' @export
#'
rswat_ohg = function(i = 1L,
                     obj = 'sdc',
                     name = 'tot',
                     revert = FALSE,
                     quiet = FALSE,
                     .db = .rswat_db) {

  # name of the output file. file.cio will also be modified at the end
  new_fn = 'object.prt'

  # construct a comment line to print to the txt file
  comment_txt = paste('object.prt: written by rswat on', Sys.time())

  # construct id key 'numb' carefully as duplicated fields can make the SWAT+ executable crash!
  id_txt = as.character( 1:max( sapply(list(obj, i, name), length) ) )

  # construct the data table to write
  write_df = data.frame(numb = id_txt,
                        obtyp = obj,
                        obtypno = as.character(i),
                        hydtyp = name) |>
    dplyr::mutate(filename = paste0(paste(obtyp, obtypno, hydtyp, sep='_'), '.ohg'))

  # render the table as text and append to comment line
  write_txt = print.data.frame(write_df,
                               row.names=FALSE,
                               width=.rswat_gv_precision('n_line'),
                               max=.rswat_gv_precision('n_all')) |> utils::capture.output()

  # debugging: fix double-escaped backslashes
  write_txt = c(comment_txt, gsub('\\\\', '\\', write_txt, fixed=TRUE))

  # check if the file exists already and print some feedback to user
  write_path = file.path(.db$get_swat_dir(), new_fn)
  msg_write = ifelse(revert, 'deleting', 'writing') |> paste(write_path)
  fn_exists = file.exists(write_path)
  if( fn_exists )
  {
    if(!quiet) message(msg_write)
    if(revert) unlink(write_path)

  } else {

    if(revert) warning('object.prt not found')
    if(!quiet) message(msg_write)
  }

  # print to output file
  if(!revert) writeLines(write_txt, write_path)

  # open file.cio, add 'object.prt' pointer
  file.cio = rswat_open('file.cio', quiet=quiet)
  is_sim = file.cio[[1L]] == 'simulation'
  file.cio[['V4']][is_sim] = ifelse(revert, 'null', 'object.prt')

  # write changes and refresh database
  rswat_write(file.cio, overwrite=TRUE, quiet=quiet)
  rswat_open(f='file.cio', refresh=FALSE, quiet=TRUE, output=FALSE, .db=.db)
  .db$refresh_cio_df(quiet=FALSE)

  # load new file, or, in revert mode, return nothing
  if(revert) return(invisible())
  rswat_open('object.prt', refresh=TRUE, quiet=TRUE, .db=.db)

}

#' Configure simulation time for a SWAT+ simulation
#'
#' This modifies 'print.prt' and 'time.sim' on disk to configure a simulation over the
#' requested dates. Outputs are printed starting `nyskip` years after the starting date.
#'
#' @param dates integer, vector, or data frame indicating the dates to simulate
#' @param nyskip integer, the number of years to skip in printed output
#'
#' @return something
#' @export
#'
rswat_time = function() {

  # ARGUMENTS:
  #
  # `dates`: integer, vector, or data frame indicating the dates to simulate (see DETAILS)
  # `nyskip`:
  # `daily`: boolean, indicates to make adjustments for daily output (see DETAILS)
  # 'quiet': boolean, indicating to suppress console messages
  #
  # RETURN:
  #
  # named (Date class) vector of start and end dates currently set in 'time.sim'
  #
  # DETAILS:
  #

  #
  # If `daily == TRUE` then 'step' (in 'time.sim') is set to run daily time steps,
  # `interval` is set for daily printing, and 'print.prt' is modified to request
  # all of the daily files, and none of the non-daily ones.
  #
  # Argument 'dates' can be a dataframe containing a 'date' column or a vector of dates
  # (must be coercible with `as.Date`); or an integer indicating the number of timesteps,
  # in which case the existing start date from the file is reused, and the end date is
  # modified accordingly to get the desired number of steps.

  # grab up-to-date copies of 'time.sim' and 'print.prt'
  time.sim = rswat_open('time.sim', quiet=quiet, reload=TRUE)
  print.prt = rswat_open('print.prt', quiet=quiet, reload=TRUE)

  # extract currently assigned start/end dates
  date.start = as.Date(paste(time.sim[,c('yrc_start', 'day_start')], collapse='-'), '%Y-%j')
  date.end = as.Date(paste(time.sim[,c('yrc_end', 'day_end')], collapse='-'), '%Y-%j')

  # if requested, modify 'step', 'interval', and print table for daily timestep outputs
  if( daily )
  {
    # other possible values for step include 1, 24, 96, 1440
    time.sim$step = 0

    # indicates to print 1 output row per timestep
    print.prt[[1]]$interval = 1

    # toggle all of the print options off and write this change to disk
    print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'n'
    print.prt[[5]]['daily'] = 'y'
    rswat_write(print.prt[[5]], preview=FALSE, quiet=quiet)
  }

  # modify 'nyskip' as requested
  print.prt[[1]]$nyskip = nyskip

  # if no dates supplied skip ahead
  if( !is.null(dates) )
  {
    # handle dataframe input
    if( is.data.frame(dates) ) dates = dates$date

    # handle numeric input
    if( is.numeric(dates) )
    {
      # coerce to integer and interpret as number of days
      dates = as.integer(dates)
      if( length(dates) > 1 ) stop('numeric-type `dates` input must be a single integer')

      # modify end date accordingly
      date.end = date.start + dates

    } else {

      # for all other input classes, attempt to coerce to Date
      dates = as.Date( dates[!is.na(dates)] )
      if( length(dates) == 0 ) stop('`dates` could not be interpreted as Date object')

      # assign new start and end dates
      date.start = min(dates)
      date.end = max(dates)

    }

    # build dataframe with new dates
    pars.tochange = c(day_start = as.integer(format(date.start, '%j')),
                      yrc_start = as.integer(format(date.start, '%Y')),
                      day_end = as.integer(format(date.end, '%j')),
                      yrc_end = as.integer(format(date.end, '%Y')))

    # progress message
    if( !quiet ) cat('writing to time.sim and print.prt...')

    # make these changes in 'time.sim' and overwrite on disk
    time.sim[ names(pars.tochange) ] = pars.tochange
    rswat_write(time.sim, preview=F, quiet=TRUE)

    # make these changes in 'print.prt' and overwrite on disk
    print.prt[[1]][ names(pars.tochange) ] = pars.tochange
    rswat_write(print.prt[[1]], preview=F, quiet=TRUE)
    if( !quiet ) cat('done\n')
  }

  # return the current simulation date range
  return( c(start=date.start, end=date.end) )

}
