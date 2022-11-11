
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
#' can be large and slow to parse. Set `exclude=""` to load everything.
#'
#' @param swat_dir character, path to the SWAT+ project directory (should contain "file.cio")
#' @param exe_dir character, path to the SWAT+ simulation executable (should have extension ".exe")
#' @param load_all logical, indicating to load all configuration files except as specified in `exclude`
#' @param exclude character vector, regular expressions for groups/names to not load
#' @param reset logical, indicates to re-initialize rswat (erases any loaded data)
#' @param .db rswat_db object, for internal use
#'
#' @return SWAT+ directory (character), or a logical indicating if new directory is different
#' @export
#'
rswat = function(swat_dir = NULL,
                 exe_dir = NULL,
                 load_all = FALSE,
                 exclude = .rswat_gv_exclude(),
                 reset = FALSE,
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
  rswat_open('file.cio', quiet=TRUE, .db=.db)

  # load other files on request
  f_df = rswat_files(what=c('file', 'group'), include='known', .db=.db)
  is_available = !( ( f_df[['file']] %in% exclude ) | (f_df[['group']] %in% exclude) )
  if(load_all)
  {
    # load them or else warn if there aren't any left after exclusions
    if( any(is_available))  { rswat_open(f_df[['file']][is_available]) } else {

      warning(paste('no other known files found in', swat_dir))
    }
  }

  # run check to print info then fetch a copy of files list
  rswat_check(quiet=quiet, .db=.db)
  cio_out = rswat_files(refresh=FALSE, include='loaded', .db=.db)

  # return nothing if no directory is assigned
  if(nrow(cio_out) == 0) return(invisible())

  # newline to separate console print-out of return data frame
  if(!quiet) cat('\n')
  return(cio_out)
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
#' @param include character, one of 'loaded', 'known', 'all'
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
#' @param quiet logical, suppresses console output and returns results invisibly
#'
#' @return a data frame or a list of them, the parameter tables in the config file
#' @export
rswat_open = function(f=NULL, quiet=FALSE, .db=.rswat_db)
{
  # make sure the project directory is assigned
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # when called without arguments, return a list of known config files
  if(is.null(f))
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

    if(quiet) return(invisible(f_available))
    return(f_available)
  }

  # recursive call to open multiple files in a loop and return results in list
  if( length(f) > 1 )
  {
    # attempt open the files then check for success
    .db$open_config_batch(f, quiet=quiet)
    is_loaded = f %in% rswat_files(what='file', include='loaded', .db=.db)
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


#' Search SWAT+ config files for a keyword in variable names
#'
#' Returns a data frame of information on matches of `pattern` against SWAT+ variable
#' names. The default `'*'` matches everything, resulting in a data frame summarizing
#' ALL fields in the specified file(s).
#'
#' `fuzzy = -1` is for exact matches only, `fuzzy = 0` includes substring matches, and
#' `fuzzy > 0` includes approximate substring matches. See `?rswat_string_distance`.
#'
#' By default the function searches all loaded files. Otherwise, the function searches
#' in the subset of files specified by `include`, loading missing files as needed.
#' Get a list of files and their load state with `rswat_open()`.
#'
#' @param pattern character vector, the string(s) to search for
#' @param fuzzy numeric, specifying tolerance for approximate matches (see details)
#' @param trim integer between 0-3, higher means more simplification in results
#' @param n_max positive integer, the maximum number of search results to return
#' @param include character vector, file (or file group) names to include in search
#' @param .db rswat_db object, for internal use
#'
#' @return A data frame of information about SWAT+ names matching the search pattern(s)
#' @export
rswat_find = function(pattern = '*',
                      fuzzy = -1,
                      trim = 2,
                      n_max = 10L,
                      include = rswat_files(what='file', include='known', .db=.db),
                      .db=.rswat_db)
{
  # load any missing files
  is_loaded = include %in% rswat_files(what='file', include='known', .db=.db)
  if( any(!is_loaded) ) rswat_open(include[!is_loaded], .db=.db)

  # collapse vector pattern to series of OR
  pattern = paste(unique(pattern), collapse='|')

  # find index for this file in fields info data-frame, copy names list
  is_included = (.db$line_df[['file']] %in% include) & .db$line_df[['header']]

  # copy the names list to search and pass it to string distance function
  name_included = .db$line_df[['name']][is_included]
  dist_result = rswat_string_dist(pattern, name_included)

  # initialize results vector to exact matches only
  is_exact = dist_result == 0
  idx_result = which(is_exact)

  # add sub-string matches on request
  if( !(fuzzy < 0) )
  {
    # exclude exact matches and sort results, then add to results stack
    is_sub = !is_exact & (dist_result < 1)
    idx_result = c(idx_result, which(is_sub)[ order( dist_result[is_sub] ) ])

    # approximate search mode
    if( fuzzy > 0 )
    {
      # sort remaining elements into bins of equal distance
      is_approx = !is_exact & !is_sub
      approx_bins = unique( sort( dist_result[is_approx]) )

      # find matching elements from the best bins, add sorted indices to stack
      is_approx_match = dist_result %in% approx_bins[ seq_along(approx_bins) < ceiling(fuzzy) ]
      idx_result = c(idx_result, which(is_approx_match)[ order( dist_result[is_approx_match] ) ])
    }
  }

  # extract all available info on matches, append distance scores
  line_df_sub = .db$line_df[which(is_included)[idx_result],]
  line_df_sub[['distance']] = dist_result[idx_result]

  # count results
  n_results = nrow(line_df_sub)
  if(n_results == 0)
  {
    message('no results found. Try increasing fuzzy or loading more files')
    return(invisible())
  }

  # warn about results omitted from output by request (if any)
  line_df_omit = tail(line_df_sub, pmax(0, n_results - n_max))
  n_omit = nrow(line_df_omit)
  n_exact_omit = sum(line_df_omit[['distance']] == 0)
  if(n_omit > 0) message(paste0(n_omit, ' results not shown (', n_exact_omit, ' exact)'))

  # omit certain attributes depending on trim level
  if( trim == 1 ) line_df_sub = line_df_sub[, .rswat_gv_find_trim_1()]
  if( trim > 1) line_df_sub[['distance']] = round(line_df_sub[['distance']], 2)
  if( trim == 2 ) line_df_sub = line_df_sub[, .rswat_gv_find_trim_2()]
  if( trim > 2 ) line_df_sub = line_df_sub[, .rswat_gv_find_trim_3()]

  # clean up row names and return subset of attributes
  rownames(line_df_sub) = NULL
  return(head(line_df_sub, n_max))
}















