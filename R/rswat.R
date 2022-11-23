
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
#' @param exe_path character, path to the SWAT+ simulation executable (should have extension ".exe")
#' @param load_all logical, indicating to load all configuration files except as specified in `exclude`
#' @param exclude character vector, regular expressions for groups/names to not load
#' @param reset logical, indicates to re-initialize rswat (erases any loaded data)
#' @param .db rswat_db object, for internal use
#'
#' @return A data frame of information on the SWAT+ files in `swat_dir`
#' @export
#'
rswat = function(swat_dir = NULL,
                 exe_path = NULL,
                 include = c('config', 'weather'),
                 exclude = .rswat_gv_exclude(),
                 n_max = 5L,
                 quiet = FALSE,
                 reset = FALSE,
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

  # read file.cio to add type and group labels then refresh again
  rswat_open(f='file.cio', quiet=TRUE, .db=.db)
  .db$refresh_cio_df()

  # load other files on request
  if( !is.null(include) )
  {
    # include has precedence over exclude in conflicts
    is_conflicted = exclude %in% include
    if( any(is_conflicted) ) exclude = exclude[!is_conflicted]

    # get a data frame of available files
    f_df = rswat_files(loaded = NA,
                       known = TRUE,
                       what = c('file', 'group', 'type', 'exists'),
                       n_max = NA,
                       refresh = FALSE,
                       quiet = TRUE,
                       .db = .db)

    # identify rows to include
    is_excluded = apply(f_df, 1L, \(x) any(x %in% exclude) )
    is_included = apply(f_df, 1L, \(x) any(x %in% include) )
    is_available = is_included & !is_excluded & f_df[['exists']]

    # load included files or else warn if there aren't any left after exclusions
    if( !any(is_available))  { warning('no additional files loaded') } else {

      rswat_open(f_df[['file']][is_available], refresh=FALSE, quiet=quiet)
    }
  }

  # run check to print info then fetch a copy of files list
  rswat_check(quiet=quiet, .db=.db)
  cio_out = rswat_files(loaded = TRUE,
                        refresh = FALSE,
                        quiet = quiet,
                        n_max = n_max,
                        .db = .db)

  # return nothing if no directory has been assigned
  if(nrow(cio_out) == 0) return(invisible())

  # return full data frame invisibly
  return(invisible(cio_out))
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

rswat_files = function(pattern = NA,
                       type = NA,
                       loaded = NA,
                       known = TRUE,
                       what = .rswat_gv_cio_show(),
                       quiet = FALSE,
                       n_max = 15L,
                       sort_by = 'standard',
                       refresh = TRUE,
                       .db = .rswat_db)
{
  # refresh files list
  if(refresh) rswat(swat_dir=.db$get_swat_dir(), include=NULL, quiet=TRUE, .db=.db)

  # copy requested subset of files data frame
  files_df = .db$get_cio_df()
  msg_extra = NULL
  if(sort_by == 'time_load')
  {
    # NAs get put at the end in this ordering
    files_df = files_df[order(files_df[['time_load']], decreasing=TRUE), ]
    msg_extra = '(sorted by load order)'
  }

  # build an index of results to exclude
  is_omitted = logical( nrow(files_df) )
  if( !is.na(loaded) ) is_omitted = is_omitted | ( files_df[['loaded']] != loaded )
  if( !is.na(known) ) is_omitted = is_omitted | ( files_df[['known']] != known )
  if( !is.na(pattern) )
  {
    is_match = apply(files_df[c('group', 'type')], 1L, \(x) any(x == pattern))
    is_omitted = is_omitted | !is_match
  }

  # handle files with NA group or type
  is_omitted[ is.na(is_omitted) ] = TRUE

  # subset of results to return invisibly later
  files_df = files_df[!is_omitted, what, drop=FALSE]
  row.names(files_df) = NULL

  # console print-out of first `n_max` rows of return data frame
  if(!quiet)
  {
    # same as no limit
    if(is.na(n_max)) n_max = Inf

    # count rows and check if we need to clip console printout
    n_included = sum(!is_omitted)
    n_show = min(n_max, n_included)
    is_clipped = n_show < n_included
    if( n_included == 0L ) { message('no results') } else {

      # when both loaded and known are TRUE (redundant), set known to NA to clean up message
      known = ifelse(is.na(loaded), known, ifelse(loaded, NA, known))

      # modifiers for message (gsub and outer paste convert NA to '')
      msg_known = gsub('NA', '', paste( paste0(c(' un', ' '), 'known')[1L + known]))
      msg_load = gsub('NA', '', paste( paste(c(' not', ''), 'currently loaded')[1L + loaded]))
      msg_pattern = ifelse(is.na(pattern), '', paste0(' ', pattern))

      # report on trimmed rows
      msg_report = paste0(n_included, msg_known, msg_pattern, ' file(s)', msg_load)
      message(msg_report)
      if(is_clipped) message(paste('showing the first',  n_show, msg_extra))

      # print messages, then the data frame
      cat('\n')
      print(head(files_df, n_show), quote=FALSE)
      if(is_clipped)
      {
        cat('...\n')
        n_omit = n_included - n_show
        msg_omit = paste(n_omit, 'result(s) not shown. Increase n_max to see more')
        if( n_omit > 0L ) message(msg_omit)
      }
    }
  }

  # returns empty data frame if nothing was found
  return( invisible(files_df) )
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
rswat_open = function(f = NULL,
                      refresh = FALSE,
                      update_stats = TRUE,
                      quiet = FALSE,
                      .db = .rswat_db)
{
  # make sure the project directory is assigned
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # when called without arguments, return a list of known config files
  if( is.null(f) )
  {
    # load the valid file name choices
    f_available = rswat_files(known = TRUE,
                              what = c('file', 'loaded'),
                              n_max = NA,
                              refresh = FALSE,
                              quiet = quiet,
                              .db = .db)

    # print choices
    if(!quiet)
    {
      # print a message about loaded and not-loaded files
      msg_loaded = paste(f_available[['file']][ f_available[['loaded']] ], collapse=', ')
      msg_not_loaded = paste(f_available[['file']][ !f_available[['loaded']] ], collapse=', ')
      all_loaded = nchar(msg_not_loaded) == 0L

      message('select a file...')
      message(ifelse(all_loaded, 'all files loaded:', 'loaded:'))
      cat(paste0(msg_loaded, '\n'))
      if(!all_loaded)
      {
        message('not loaded:')
        cat(paste0(msg_not_loaded, '\n'))
      }
    }

    return(invisible(f_available))
  }

  # overwrite files argument f with 'file' column from data frame input
  if( is.data.frame(f) ) f = f[['file']]

  # recursive call to open multiple files in a loop and return results in list
  if( length(f) > 1L )
  {
    # attempt open the files then check for success
    .db$open_config_batch(f, refresh=refresh, update_stats=update_stats, quiet=quiet)
    is_loaded = .db$is_file_loaded(f)
    if( any(!is_loaded) ) warning('files ', paste(paste(f[!is_loaded], collapse=', ')), ' not found')

    # return everything that was successfully loaded, collapsing length-1 lists
    list_out = .db$get_stor_df(f[is_loaded])
    if(length(list_out) == 1L) list_out = list_out[[1]]

  } else {

    # open a single file
    list_out = .db$open_config_file(f, refresh=refresh, update_stats=update_stats)
  }

  # collapse length-1 lists and return
  if(length(list_out) == 1L) list_out = list_out[[1]]
  if(quiet) return(invisible(list_out))
  return(list_out)
}


