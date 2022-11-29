
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
                 exe_path = NA_character_,
                 include = 'default',
                 exclude = .rswat_gv_exclude(),
                 quiet = FALSE,
                 reset = FALSE,
                 refresh = FALSE,
                 .db = .rswat_db) {

  # check that the package was loaded properly and re-initialize if requested
  rswat_check(quiet=TRUE, .db=.db)
  if(reset) .db$initialize()

  # handle calls without a project folder assigned
  old_swat_dir = .db$get_swat_dir()
  is_assigned = !is.na(old_swat_dir)
  if(!is_assigned)
  {
    msg_assigned = 'Project directory has not been assigned. Try rswat(swat_dir)'
    if( is.null(swat_dir) )
    {
      # message if rswat called before project directory is specified
      message(msg_assigned)
      return(invisible())
    }

    # this should be the first assignment of swat_dir
    old_swat_dir = rswat_validate_dpath(swat_dir, 'swat_dir')
    if( is.na(old_swat_dir) ) stop(msg_assigned)
  }

  # assign the project path and executable path (optional)
  swat_dir = .db$set_swat_dir(ifelse(is.null(swat_dir), old_swat_dir, swat_dir))
  if( !is.na(exe_path) ) .db$set_exe_path(exe_path)
  exe_path = .db$get_exe_path()

  # re-initialize the rswat_db if swat_dir was changed (skipped on initial call)
  if( old_swat_dir != swat_dir )
  {
    if(!quiet) message(paste('changed project directory to:', swat_dir))
    .db$initialize(swat_dir=swat_dir, exe_path=exe_path)
  }

  # scan for files
  .db$refresh_cio_df()

  # read file.cio to add type and group labels
  rswat_open(f='file.cio', refresh=FALSE, quiet=TRUE, .db=.db)

  # refresh again (why is this necessary?)
  .db$refresh_cio_df()

  # load other files on request
  if( !is.null(include) )
  {
    file_df = rswat_files(include=include, exclude=exclude, refresh=FALSE, quiet=TRUE, .db=.db)
    is_needed = file_df[['exists']] & !file_df[['loaded']]
    if( refresh ) is_needed = is_needed & !file_df[['loaded']]

    # load included files or else warn if there aren't any left after exclusions
    if( !any(is_needed))  { if(!quiet) message('all requested files loaded') } else {

      if(!quiet) message(paste('loading files from:', swat_dir))
      rswat_open(file_df[['file']][is_needed], refresh=refresh, quiet=quiet)
    }
  }

  # print rswat summary info
  rswat_check(quiet=quiet, .db=.db)

  # print this advice once only, when project folder is first assigned
  if(!quiet & !is_assigned) message('Browse files with rswat_files() and rswat_open()')
  return(invisible())
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
                      refresh = TRUE,
                      update_stats = TRUE,
                      quiet = FALSE,
                      output = TRUE,
                      .db = .rswat_db) {

  # make sure the project directory is assigned
  if( is.na(.db$get_swat_dir()) ) stop('Set the project directory first with rswat(swat_dir)')

  # when called without arguments, return a list of known config files
  if( is.null(f) )
  {
    # load the valid file name choices
    f_available = rswat_files(known = TRUE,
                              what = c('file', 'loaded'),
                              n = NA,
                              refresh = refresh,
                              quiet = TRUE,
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

  # check if the file(s) can be found in the SWAT+ project directory
  is_known = f %in% .db$get_cio_df(what='file', drop=TRUE)

  # recursive call to open multiple files in a loop and return results in list
  if( length(f) > 1L )
  {
    # these files requested but not found on disk
    if( !quiet & any(!is_known) ) message(paste('ignoring unknown file(s):',
                                                paste(f[!is_known], collapse=', ')))

    # attempt open the known files then check for success
    .db$open_config_batch(f[is_known], refresh=refresh, update_stats=update_stats, quiet=quiet)
    if(!quiet) cat('\n')
    is_loaded = .db$is_file_loaded(f)

    # return everything that was successfully loaded, collapsing length-1 lists
    if(output)
    {
      list_out = .db$get_stor_df(f[is_loaded])
      if(length(list_out) == 1L) list_out = list_out[[1]]
    }

  } else {

    # print suggestions if the file is not known to rswat
    if(!is_known)
    {
      if(!quiet) rswat_find(f=f)
      return(list())
    }

    # open a single file
    .db$open_config_file(f, refresh=refresh, output=FALSE, update_stats=update_stats)
    is_loaded = .db$is_file_loaded(f)
    list_out = .db$get_stor_df(f[is_loaded])
  }

  # report load failures
  is_failed = is_known & !is_loaded
  if(!quiet & any(is_failed)) message(paste0('rswat failed to load ',
                                             paste(f[is_failed], collapse=', '),
                                             '\n View the first error message with ',
                                             'rswat_files(',
                                             '"', f[which(is_failed)[1L]], '"',
                                             ', what=c("file", "error", "msg"))'))

  # return without data frames
  if(!output) return(invisible())

  # collapse length-1 lists and return
  if(length(list_out) == 1L) list_out = list_out[[1]]
  if(quiet) return(invisible(list_out))
  return(list_out)
}


#' Return a data frame of information about SWAT+ files
#'
#' If `pattern` is the name of a file in the SWAT+ project directory, the function
#' returns a one-row data frame with information about the file. If `pattern` is `NA`,
#' information on all files is returned. Otherwise all matches of `pattern` against
#' 'group' and 'type' are returned.
#'
#' `loaded` and `known` have no effect when set to `NA`, or when `pattern` is a file
#' name. Otherwise they control the subset of results to return. `loaded=TRUE`
#' restricts to files loaded already, and `loaded=FALSE` restricts to those not loaded.
#' `known=TRUE` restricts results to file types known to `rswat` (based on file name
#' and extension) and `known=FALSE` returns only unrecognized files.
#'
#' `what` controls which columns to return. Call `.rswat_gv_cio_show()` to see the
#' defaults, or `names(rswat_files(what=NULL))` to see all options.
#'
#' By default at most `n=15` rows are printed for tidiness, but the entire set of
#' results is returned invisibly. Set `quiet=TRUE` to print nothing.
#'
#' @param pattern character, the file name, group, or type of file
#' @param type character, one of 'config' (the default)...
#' @param what character vector, the column(s) to select
#' @param loaded logical, indicates to only search among loaded files
#' @param known logical, indicates to only search among recognized files
#' @param n integer, the maximum number of results to print
#' @param quiet logical, suppresses console output
#' @param refresh logical, indicates to first re-scan the directory
#' @param .db rswat_db object, for internal use
#'
#' @return the requested data frame or vector, a subset of `cio`
#' @export
rswat_files = function(pattern = NA,
                       loaded = NA,
                       known = NA,
                       include = NULL,
                       exclude = NULL,
                       what = NULL,
                       n = NULL,
                       refresh = TRUE,
                       summarize = 'file',
                       quiet = FALSE,
                       .db = .rswat_db) {

  # refresh files list
  if(refresh) rswat(swat_dir=.db$get_swat_dir(), include=NULL, quiet=TRUE, .db=.db)

  # copy requested subset of files data frame
  files_df = .db$get_cio_df()
  if( is.null(what) ) what = seq_along(files_df)

  # keep only subset of rows referred to by include and not exclude
  if( !is.null(include) )
  {
    # process shorthand in 'include'
    if( length(include) == 1L ) include = .rswat_gv_include_lu(include)

    # include has precedence over exclude in conflicts
    is_conflicted = exclude %in% include
    if( any(is_conflicted) ) exclude = exclude[!is_conflicted]
    is_excluded = apply(files_df, 1L, \(x) any(x %in% exclude) )
    is_included = apply(files_df, 1L, \(x) any(x %in% include) )
    is_left = is_included & !is_excluded
    files_df = files_df[is_left, , drop=FALSE]

    # warn if there aren't any left after exclusions
    if( !any(is_left) )
    {
      if(!quiet) message('All files excluded. Try exclude=NULL?')
      return( files_df[, what, drop=FALSE] )
    }
  }

  # check for matches against file names and return them
  is_exact = files_df[['file']] %in% pattern
  if( any(is_exact) ) { is_omitted = !is_exact } else {

    # no exact matches - build an index of results to exclude
    is_omitted = logical( nrow(files_df) )
    if( !is.na(loaded) ) is_omitted = is_omitted | ( files_df[['loaded']] != loaded )
    if( !is.na(known) ) is_omitted = is_omitted | ( files_df[['known']] != known )
    if( !is.na(pattern) )
    {
      # look for matches
      is_match = apply(files_df[c('group', 'type')], 1L, \(x) any(x == pattern))
      is_omitted = is_omitted | !is_match
    }

    # handle files with NA group or type
    is_omitted[ is.na(is_omitted) ] = TRUE
  }

  # subset of results
  files_df = files_df[!is_omitted, what, drop=FALSE]
  row.names(files_df) = NULL

  # summarize on request and return as tibble either way
  return(rswat_summarize_files(files_df = files_df,
                               loaded = loaded,
                               known = known,
                               n = n,
                               level = summarize,
                               quiet = quiet))
}


#' Print a summary of a data frame of SWAT+ file information
#'
#' A helper function for `rswat_files` to print file information in the console.
#' `level` specifies the detail level for the output. The default `level='file'`
#' returns information about every file on its own row.
#'
#' With `level='group'` each group gets a row, indicating how many files there are
#' and how many are loaded, along with a list of the file names (truncated to the
#' console width, or `max_len` if it is assigned). `level='type'` does the same but
#' with summaries of the four file types, and shows group names within each type
#' category instead of file names.
#'
#' `n` controls the maximum number of rows to print in the console (all rows are
#' returned invisibly by the function). `loaded` and `known` modify the message
#' printed about the files.
#'
#' @param files_df data frame, output from `rswat_files`
#' @param loaded logical, indicates that the files are all loaded
#' @param known logical, indicates that the files are all recognized by rswat
#' @param n integer, the maximum number of results to print
#' @param level character, what category to summarize over (see details)
#'
#' @return returns a data frame invisibly, and prints the first `n` lines of it
#' @export
rswat_summarize_files = function(files_df,
                                 loaded = NA,
                                 known = NA,
                                 n = NULL,
                                 level = 'file',
                                 quiet = FALSE) {

  # handle empty data frames
  n_files = nrow(files_df)
  if( n_files == 0L ) {

    if(!quiet) message('no results')
    return(dplyr::tibble())
  }

  # summarize groups
  if(level == 'group') files_df = files_df |>
      dplyr::mutate(group = factor(group, levels=unique(group))) |>
      dplyr::group_by(group) |>
      dplyr::summarize(type = head(unique(type), 1L),
                       loaded = paste0(sum(loaded), '/', dplyr::n()),
                       size = sum(size, na.rm=TRUE),
                       modified = max(modified, na.rm=TRUE),
                       files = paste(file, collapse=', ')) |>
      dplyr::mutate(group = as.character(group))

  # summarize types
  if(level == 'type') files_df = files_df |>
      dplyr::mutate(type = factor(type, levels=unique(type))) |>
      dplyr::group_by(type) |>
      dplyr::summarize(loaded = paste0(sum(loaded), '/', dplyr::n()),
                       size = sum(size, na.rm=TRUE),
                       modified = max(modified, na.rm=TRUE),
                       groups = paste(unique(group), collapse = ', ')) |>
      dplyr::mutate(type = as.character(type))

  # when both loaded and known are TRUE (redundant), set known to NA to clean up message
  known = ifelse(is.na(loaded), known, ifelse(loaded, NA, known))

  # modifiers for message (gsub and outer paste convert NA to '')
  msg_known = gsub('NA', '', paste( paste0(c(' un', ' '), 'known')[1L + known]))
  msg_load = gsub('NA', '', paste( paste(c(' not', ''), 'currently loaded')[1L + loaded]))
  if(!quiet)
  {
    # report the number of objects (after summarization) and return as tibble
    n_out = nrow(files_df)
    msg_report = paste0(n_out, msg_known, ' ', level, '(s)', msg_load)
    message(msg_report)
    print(dplyr::tibble(files_df), n=n)
  }

  # in quiet mode return invisibly
  return( invisible(dplyr::tibble(files_df)) )
}

