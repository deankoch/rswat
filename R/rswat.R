
#' Open a SWAT+ project directory
#'
#' Scans a SWAT+ project directory for configuration files and optionally loads them. This
#' initializes an `rswat_db` reference class object in your session, allowing SWAT+ configuration
#' files in `swat_dir` files to be listed, and their contents displayed and modified.
#'
#' When `swat_dir` is set for the first time in an R session, or when it is changed, the
#' function will scan the directory for SWAT+ files, building a data frame of information
#' about them which can be accessed with functions like `rswat_files` or `rswat_find`.
#'
#' The function invisibly returns a data frame of information on the files in `swat_dir`,
#' printing its first few rows (by default, when `quiet=TRUE`).
#'
#' Files of type 'output', 'decision_table', 'gwflow' are not loaded by default because they
#' can be large and slow to parse. Set `exclude=""` to load everything.
#'
#' @param swat_dir character, path to the SWAT+ project directory (should contain "file.cio")
#' @param exe_dir character, path to the SWAT+ simulation executable (should have extension ".exe")
#' @param load_all logical, indicating to load all configuration files except as specified in `exclude`
#' @param exclude character vector, regular expressions for groups/names to not load
#' @param reset logical, indicates to re-initialize rswat (erases any loaded data)
#' @param .db rswat_db object, for internal use
#'
#' @return A data frame of information on the SWAT+ files in `swat_dir`
#' @export
#'
rswat = function(swat_dir = NULL,
                 exe_dir = NULL,
                 load_all = FALSE,
                 exclude = .rswat_gv_exclude(),
                 reset = FALSE,
                 n_max = 10,
                 quiet = FALSE,
                 .db = .rswat_db)
{
  # check that the package was loaded properly and re-initialize if requested
  rswat_check(quiet=TRUE, .db=.db)
  if(reset) .db$initialize()

  # change or initialize the project directory
  if( is.null(swat_dir) ) swat_dir = .db$get_swat_dir()
  .db$set_swat_dir(swat_dir)
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # change or initialize the executable path (optional)
  if( is.null(exe_path) ) exe_path = .db$get_exe_path()
  .db$set_exe_path(exe_path)

  # scan for files
  .db$refresh_cio_df()

  # read file.cio to add type and group labels
  rswat_open(f='file.cio', quiet=TRUE, .db=.db)

  # load other files on request
  f_df = rswat_files(what=c('file', 'group'), include='cio', .db=.db)
  is_available = !( ( f_df[['file']] %in% exclude ) | (f_df[['group']] %in% exclude) )
  if(load_all)
  {
    # load them or else warn if there aren't any left after exclusions
    if( !any(is_available))  { warning('no additional files loaded') } else {

      rswat_open(f_df[['file']][is_available])
    }
  }

  # run check to print info then fetch a copy of files list
  rswat_check(quiet=quiet, .db=.db)
  cio_out = rswat_files(refresh=FALSE, include='loaded', .db=.db)

  # return nothing if no directory is assigned
  if(nrow(cio_out) == 0) return(invisible())

  # console print-out of first `n_max` rows of return data frame
  if(!quiet)
  {
    # count rows and check if we need to trim
    n_cio = nrow(cio_out)
    n_print = pmin(n_cio, n_max)
    is_cropped = n_print < n_cio

    # report on trimmed rows
    msg_shown = ifelse(is_cropped, paste0('(displaying the first ',  n_print, ')'), '')
    message( paste(n_cio, 'file(s) loaded', msg_shown, '\n') )

    # print the data frame
    print(head(cio_out, n_print), quote=FALSE)
    cat(paste0(ifelse(is_cropped, '...', ''), '\n'))
    if(is_cropped) message('show all files with rswat_files()')
  }

  # return full data frame invisibly
  return(invisible(cio_out))
}

#' Open a SWAT+ project file with rswat
#'
#' Returns data frame, or a list of them, corresponding to the data values
#' found in the SWAT+ file.
#'
#' @param f character vector, the file name(s) to select
#' @param .db rswat_db object, for internal use
#' @param quiet logical, suppresses console output and returns results invisibly
#'
#' @return a data frame or a list of them, the parameter tables in the config file
#' @export
rswat_open = function(f=NULL, quiet=FALSE, .db=.rswat_db)
{
  # make sure the project directory is assigned
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # when called without arguments, return a list of known config files
  if( is.null(f) )
  {
    # load the valid file name choices
    f_available = rswat_files(what=c('file', 'loaded'), include='known', .db=.db)
    if(!quiet)
    {
      # print a message about loaded and not-loaded files
      f_msg1 = paste(subset(f_available, loaded==TRUE)[['file']], collapse=', ')
      f_msg2 = paste(subset(f_available, loaded==FALSE)[['file']], collapse=', ')
      all_loaded = nchar(f_msg2) == 0

      message('select a file...')
      message(ifelse(all_loaded, 'all files loaded:', 'loaded:'))
      cat(paste0(f_msg1, '\n'))
      if(!all_loaded)
      {
        message('not loaded:')
        cat(paste0(f_msg2, '\n'))
      }
    }

    return(invisible(f_available))
  }

  # recursive call to open multiple files in a loop and return results in list
  if( length(f) > 1 )
  {
    # attempt open the files then check for success
    .db$open_config_batch(f, quiet=quiet)
    is_loaded = .db$is_file_loaded(f)
    if( any(!is_loaded) ) warning('files ', paste(paste(f[!is_loaded], collapse=', ')), 'not found')

    # return everything that was successfully loaded, collapsing length-1 lists
    list_out = .db$get_stor_df(f[is_loaded])
    if(length(list_out) == 1) list_out = list_out[[1]]

  } else {

    # open a single file
    list_out = .db$open_config_file(f)
  }

  # collapse length-1 lists and return
  if(length(list_out) == 1) list_out = list_out[[1]]
  if(quiet) return(invisible(list_out))
  return(list_out)
}

#' Return a data frame of information about SWAT+ files
#'
#' TODO: details here
#'
#' `f` filters rows by specifying file name(s) (default `NULL` selects all),
#'
#' @param what character vector, the column(s) to select
#' @param f character vector, the file name(s) to select
#' @param type character, one of 'config' (the default)...
#' @param include character, one of 'loaded', 'cio', 'known', 'all'
#' @param refresh logical, indicates to re-scan the directory
#' @param .db rswat_db object, for internal use
#'
#' @return the requested data frame or vector, a subset of `cio`
#' @export

rswat_files = function(f=NULL,
                       what=.rswat_gv_cio_show(),
                       type='config',
                       include='known',
                       refresh=FALSE,
                       .db=.rswat_db)
{
  # NULL what indicates to select all
  if(is.null(what)) what = names(rswat_scan_dir())

  # check that the package was loaded properly
  rswat_check(quiet=TRUE, .db=.db)

  # refresh files list then
  if(refresh) rswat(refresh=TRUE, quiet=TRUE, .db=.db)

  # find requested subset and return empty data frame if nothing was found
  cio_out = .db$get_cio_df(f=f)
  if(include=='loaded') cio_out = subset(cio_out, loaded==TRUE)
  if(include=='known') cio_out = subset(cio_out, known==TRUE)
  if(include=='cio') cio_out = subset(cio_out, !is.na(group))
  if(nrow(cio_out) == 0) return(cio_out)
  cio_out = subset(cio_out[, what], type==type)
  return( cio_out )
}





#' Get or set the SWAT+ simulation time parameters
#'
#' Helper function for reading or setting up parameters SWAT+ parameters
#' controlling the time steps executed by the simulator, as well as the
#' requested time steps (and variables) to print to output files.
#'
#' These parameters are located in two files:
#'
#' * "time.sim" controls the simulation start date, end date, and step size
#' * "print.prt" controls which output variables get printed, and over which period
#'
#' Currently the function just reports the existing dates, along with a
#' text string indicating the simulation time step.
#'
#' #' WORK IN PROGRESS
#' TODO: add options for toggling different output files
#' TODO: add write functionality
#' TODO: incorporate warm-up period
#' TODO: validate time intervals
#'
#' @param sim vector of start/end dates for the simulation (or NULL)
#' @param prt vector of start/end dates for the printed output (or NULL)
#' @param step simulation time step size: one of 'daily', 'twice_daily', 'hourly'
#' @param step_prt integer, output step size (in days)
#' @param .db rswat_db object, for internal use
#'
#' @return a list containing the currently assigned date ranges and step size
#' @export
rswat_time = function(sim=NULL, prt=NULL, step=NULL, step_prt=1L, .db=.rswat_db)
{
  # look-up table of step size codes
  step_lu = .rswat_gv_step_codes()

  # convert step to integer
  if( !is.null(step) )
  {
    # sub-daily step size (the last entry in step_lu) is unsupported
    msg_step_options = paste(head(names(step_lu), -1), collapse=', ')
    step = step_lu[step]
    if( is.na(step) ) stop('step must be one of: ', msg_step_options)
  }

  # load the files
  print_prt = rswat_open('print.prt', .db=.db)
  time_sim = rswat_open('time.sim', .db=.db)

  # extract dates info from both files
  dates_as_int = list(

    # simulation start date
    sim_start = c(j=time_sim[['day_start']],
                  yr=time_sim[['yrc_start']]),

    # simulation end date
    sim_end = c(j=time_sim[['day_end']],
                yr=time_sim[['yrc_end']]),

    # output start date
    print_start = c(j=print_prt[[1]][['day_start']],
                    yr=print_prt[[1]][['yrc_start']]),

    # output end date
    print_end = c(j=print_prt[[1]][['day_end']],
                  yr=print_prt[[1]][['yrc_end']])
  )

  # text strings describing time steps
  n_days_prt = print_prt[[1]][['interval']]
  prt_step_msg = ifelse(n_days_prt==1, 'daily', paste('every', n_days_prt, 'days'))
  idx_step = match(time_sim[['step']], step_lu)
  if(is.na(idx_step)) idx_step = match(NA, step_lu)
  sim_step_msg = names(step_lu)[idx_step]

  # convert to date objects and return in a list with step size
  dates_as_Date = setNames(rswat_date_conversion(dates_as_int), names(dates_as_int))
  list(sim = dates_as_Date[c('sim_start', 'sim_end')],
       prt = dates_as_Date[c('print_start', 'print_end')],
       step = sim_step_msg)


}







