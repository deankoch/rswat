
#' Open a SWAT+ project directory
#'
#' Scans a SWAT+ project directory for configuration files and optionally loads them. This
#' initializes an `rswat_db` reference class object in your session, allowing SWAT+ configuration
#' files in `swat_dir` files to be listed, and their contents displayed and modified.
#'
#' When `swat_dir` is set for the first time in an R session, or when it is changed, the
#' function will scan the directory for SWAT+ files, building a data frame of information
#' about them which can be accessed with functions like `rswat_files` or `rswat_find`.
#' This information can be updated with `rswat_refresh` or `rswat_load`.
#'
#' The function returns a data frame of information on the files in `swat_dir`.
#'
#' Files of type 'output', 'decision_table', 'gwflow' are not loaded by default because they
#' can be large and slow to parse. Set `ignore=""` to load everything.
#'
#' @param swat_dir character, path to the SWAT+ project directory (should contain "file.cio")
#' @param load_all logical, indicating to load all configuration files except as specified in `ignore`
#' @param ignore character vector, regular expressions for groups/names to not load
#' @param reset logical, indicates to re-initialize rswat (erases any loaded data)
#' @param .db rswat_db object, for internal use
#'
#' @return SWAT+ directory (character), or a logical indicating if new directory is different
#' @export
#'
rswat = function(swat_dir=NULL,
                 load_all=FALSE,
                 ignore=.rswat_gv_ignore(),
                 reset = FALSE,
                 quiet=FALSE,
                 .db=.rswat_db)
{
  # check that the package was loaded properly and rest if requested
  rswat_check(quiet=TRUE, .db=.db)
  if(reset) .db$initialize()

  # change or initialize the project directory
  if( is.null(swat_dir) ) swat_dir = .db$get_swat_dir()
  .db$set_swat_dir(swat_dir)
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # scan for files and read file.cio to add type and group labels
  .db$refresh_cio_df()
  if( .db$get_cio_df(f='file.cio')[['exists']] )
  {
    # load the file (calls parser and updates files list)
    .db$open_config_file('file.cio', output=FALSE)

  } else {

    # warn about missing file.cio
    if(!quiet) warning(paste('file.cio not found in project directory:', swat_dir))
  }

  # load files if requested
  if(load_all) .db$open_config_batch(ignore=ignore, quiet=quiet)

  # refresh files list, run check to print info
  cio_out = rswat_files(refresh=FALSE, .db=.db)
  rswat_check(quiet=quiet, .db=.db)

  # return nothing if no directory assigned
  if(nrow(cio_out) == 0) return(invisible())

  # newline to separate console print-out of return data frame
  if(!quiet) cat('\n')
  return(cio_out)
}


#' Return a data frame of information about SWAT+ files
#'
#' Convenience function for grabbing subsets of the file information data frame
#' `cio` located at `.rswat[['dir']][[swat_dir]][['cio']]`.
#'
#' Call this without arguments to get a complete copy of the `cio` data frame
#' Specify the column(s) to return with `what`. When `what` is a single name, the
#' function returns the requested data in a vector; When `what` is a vector, it
#' returns a data frame. When `what=NULL`, all columns are returned.
#'
#' `refresh=TRUE` causes the SWAT+ directory to be scanned for changes
#' and `cio` rebuilt with the latest file information.
#'
#' `f` filters rows by specifying file name(s) (default `NULL` selects all),
#'
#' @param what character vector, the column(s) to select
#' @param f character vector, the file name(s) to select
#' @param type character, one of 'config' (the default)...
#' @param include character, one of 'loaded', 'known', 'all'
#' @param refresh logical, indicates to re-scan the directory
#' @param .db rswat_db object, for internal use
#'
#' @return the requested data frame or vector, a subset of `cio`
#' @export

rswat_files = function(f=NULL,
                       what=.rswat_gv_cio_show(),
                       type='config',
                       include='loaded',
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
  if(nrow(cio_out) == 0) return(cio_out)
  cio_out = subset(cio_out[, what], type==type)
  return( cio_out )
}


#' Open a SWAT+ project file with rswat
#'
#' Returns data frame, or a list of them, corresponding to the data values
#' found in the SWAT+ file.
#'
#' @param f character vector, the file name(s) to select
#' @param .db rswat_db object, for internal use
#'
#' @return a data frame or a list of them
#' @export
rswat_open = function(f=NULL, quiet=FALSE, .db=.rswat_db)
{
  # make sure the project directory is assigned
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # when called without arguments, return a list of known config files
  if(is.null(f))
  {
    # load the valid file name choices
    f_options = rswat_files(what=c('file', 'loaded'), include='known', .db=.db)
    if(!quiet)
    {
      # print a message about loaded and not-loaded files
      f_msg1 = paste(subset(f_options, loaded==TRUE)[['file']], collapse=', ')
      f_msg2 = paste(subset(f_options, loaded==FALSE)[['file']], collapse=', ')
      all_loaded = nchar(f_msg2) == 0

      message(ifelse(all_loaded, 'all files loaded:', 'loaded:'))
      cat(paste0(f_msg1, '\n'))
      if(!all_loaded)
      {
        message('not loaded:')
        cat(paste0(f_msg2, '\n'))
      }
    }

    return(invisible(f_options))
  }

  list_out = .db$open_config_file(f)
  if(length(list_out) == 1) list_out = list_out[[1]]
  return(list_out)
}


#' Get or set the SWAT+ simulation time parameters
#'
#' WORK IN PROGRESS
#'
#' Helper function for reading or setting up parameters SWAT+ parameters
#' controlling the time steps executed by the simulator, as well as the
#' requested time steps (and variables) to print to output files.
#'
#' These parameters are located in two files:
#'
#' * "time.sim" controls the simulation start date, end date, and step size
#' * "print.prt" controls which output variables get printed
#'
#'
rswat_time = function(sim=NULL, prt=NULL, step=NULL, .db=.rswat_db)
{
  step_lu = c(daily=0L)

  # convert step to integer
  if( !is.null(step) )
  {
    step = step_lu[step]
    if( is.na(step) ) stop('step must be one of:', paste(names(step_lu), collapse=', '))
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

  # convert to date objects
  dates_as_Date = setNames(rswat_date_conversion(dates_as_int), names(dates_as_int))
  list(sim = dates_as_Date[c('sim_start', 'sim_end')],
       sim_step = names(step_lu)[match(time_sim[['step']], step_lu)],
       prt_step = names(step_lu)[match(print_prt[[1]][['interval']], step_lu)],
       prt = dates_as_Date[c('print_start', 'print_end')])


}



















