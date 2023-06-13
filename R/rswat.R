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

  # refresh again to catch files mentioned in file.cio but missing on disk
  .db$refresh_cio_df(quiet=FALSE)

  # load other files on request
  if( !is.null(include) )
  {
    file_df = rswat_files(include=include, exclude=exclude, refresh=FALSE, quiet=TRUE, .db=.db)
    is_needed = !file_df[['loaded']]

    # load included files or else warn if there aren't any left after exclusions
    if( !any(is_needed))  { if(!quiet) message('all requested files loaded') } else {

      #if(!quiet) message(paste('loading files from:', swat_dir))
      rswat_open(file_df[['file']][is_needed], refresh=refresh, quiet=quiet)
    }
  }

  # print rswat summary info
  rswat_check(quiet=quiet, .db=.db)

  # print this advice once only, when project folder is first assigned
  if(!quiet & !is_assigned) message('browse files with rswat_files() and rswat_open()')
  return(invisible())
}


#' Open a SWAT+ project file with rswat
#'
#' Returns data frame corresponding to the data values in the SWAT+ file (or a list of them).
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
  if( is.null(f) ) {

    # load the valid file name choices
    f_available = rswat_files(known = TRUE,
                              refresh = refresh,
                              quiet = TRUE,
                              .db = .db)

    # print choices
    if(!quiet) {

      # print a message about loaded and not-loaded files
      msg_loaded = paste(f_available[['file']][ f_available[['loaded']] ], collapse=', ')
      msg_not_loaded = paste(f_available[['file']][ !f_available[['loaded']] ], collapse=', ')
      all_loaded = nchar(msg_not_loaded) == 0L

      message('select a file...')
      message(ifelse(all_loaded, 'all files loaded:', 'loaded:'))
      cat(paste0(msg_loaded, '\n'))
      if(!all_loaded) {

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
    if(output) {

      list_out = .db$get_stor_df(f[is_loaded])
      if(length(list_out) == 1L) list_out = list_out[[1]]
    }

  } else {

    # the file is not known to rswat
    if(!is_known) {

      if(!quiet) message( paste('file', f, 'not found in ', .db$get_swat_dir()) )
      return(invisible())
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
                                             '.rswat_db$get_cio_df(f=',
                                             '"', f[which(is_failed)[1L]], '"',
                                             ', what=c("file", "error", "msg"))'))

  # return without data frames
  if(!output) return(invisible())

  # collapse length-1 lists and return
  if(length(list_out) == 1L) list_out = list_out[[1]]
  if(quiet) return(invisible(list_out))
  return(list_out)
}


#' Return a tibble with information about SWAT+ files
#'
#' By default (`pattern=NA`) the function returns information on all files in the current
#' `rswat` project directory. Set `pattern` to the name of a file, group, or type, to
#' return information on those files only.
#'
#' Aggregate results by changing `level` from 'file' to 'group' or 'type'.
#'
#' Filter results by specifying specific file name(s), group(s), or type(s) in
#' `include` and/or `exclude`. By default all files are included. Set `known=TRUE` to
#' exclude files unknown to `rswat` based on file name(s) and extension(s). Set it to
#' `FALSE` to exclude recognized files and how only unrecognized ones.
#'
#' @param pattern character, the file name, group, or type of file
#' @param level character, either 'file', 'type', or 'group'
#' @param known logical, indicates to only search among recognized files
#' @param refresh logical, indicates to first re-scan the directory
#' @param quiet logical, suppresses console output
#' @param .db rswat_db object, for internal use
#'
#' @return a tibble summarizing the requested files
#' @export
rswat_files = function(pattern = NA,
                       level = 'file',
                       include = NULL,
                       exclude = NULL,
                       known = NA,
                       refresh = TRUE,
                       quiet = FALSE,
                       .db = .rswat_db) {

  # refresh files list and get a complete copy of files data frame
  if(refresh) rswat(swat_dir=.db$get_swat_dir(), include=NULL, quiet=TRUE, .db=.db)
  files_df = .db$get_cio_df()

  # translate shorthand keywords for 'include' or set default (all)
  if( is.null(include) ) include = files_df[['file']]
  if( length(include) == 1L ) include = .rswat_gv_include_lu(include)

  # include has precedence over exclude in conflicts
  nm_check = c('file', 'type', 'group')
  is_conflicted = exclude %in% include
  if( any(is_conflicted) ) exclude = exclude[!is_conflicted]

  # apply both filters and overwrite files_df with result
  is_excluded = apply(files_df[nm_check], 1L, \(x) any(x %in% exclude) )
  is_included = apply(files_df[nm_check], 1L, \(x) any(x %in% include) )
  is_left = is_included & !is_excluded
  files_df = files_df[is_left, , drop=FALSE]

  # warn if 0 results after filtering
  if( !any(is_left) ) {

    # returns empty tibble with correct column names
    if(!quiet) message('All files excluded. Try setting include=NULL and/or exclude=NULL')

    # reorder columns for output
    return(rswat_summarize_files(files_df, level, quiet, .db))
  }

  # first check for matches against file names
  is_omitted = !( files_df[['file']] %in% pattern )

  # if no exact matches we look for approximate matches
  if( all(is_omitted) ) {

    # initialize to omit none
    is_omitted = !is_omitted

    # build an index of results to filter
    if( !is.na(known) ) is_omitted = is_omitted | ( files_df[['known']] != known )
    if( !is.na(pattern) ) {

      # look for matches
      is_match = apply(files_df[c('group', 'type')], 1L, \(x) any(x == pattern))
      is_omitted = is_omitted | !is_match
    }

    # this handles files with NA group or type
    is_omitted[ is.na(is_omitted) ] = TRUE
  }

  # filter to matches with pattern
  files_df = files_df[!is_omitted,]
  row.names(files_df) = NULL

  # summarize by level and reorder columns for output
  return(rswat_summarize_files(files_df, level, quiet, .db))
}


#' Print a summary of a data frame of SWAT+ file information
#'
#' A helper function for `rswat_files` to print file information in the console.
#' `level` specifies the detail level for the output. The default `level='file'`
#' returns information about every file on its own row.
#'
#' With `level='group'` each group gets a row, indicating how many files there are
#' and how many are loaded, along with a list of the file names. `level='type'` does
#' the same but with summaries of the four file types, and group names within each type
#' category instead of file names. In both cases, the time modified field indicates the
#' latest modification within the category
#'
#' @param files_df data frame, output from `rswat_files`
#' @param level character, the category to summarize over (see details)
#' @param quiet logical, suppresses console messages
#'
#' @return returns a data frame invisibly, and prints the first `n` lines of it
#' @export
rswat_summarize_files = function(files_df,
                                 level = 'file',
                                 quiet = FALSE,
                                 .db = .rswat_db) {

  # preferred column order for output
  nm_print = .rswat_gv_cio_show()

  # handle empty data frames
  n_files = nrow(files_df)
  if( n_files == 0L ) {

    if(!quiet) message('no results')
    return( dplyr::tibble(rswat_order_columns(files_df, nm_print)) )
  }

  # wrapper for bas::max() that doesn't fail on length zero input
  my_max = function(x) {

    if( !all(is.na(x)) & ( length(x) > 0 ) ) { max(x, na.rm=TRUE) } else { as.POSIXct(NA) }
  }

  # summarize groups
  if(level == 'group') files_df = files_df |>
    dplyr::mutate(group = factor(group, levels=unique(group))) |>
    dplyr::group_by(group) |>
    dplyr::summarize(type = head(unique(type), 1L),
                     loaded = paste0(sum(loaded), '/', dplyr::n()),
                     size = sum(size, na.rm=TRUE),
                     modified = my_max(modified),
                     files = paste(file, collapse=', ')) |>
    dplyr::mutate(group = as.character(group))

  # summarize types
  if(level == 'type') files_df = files_df |>
      dplyr::mutate(type = factor(type, levels=unique(type))) |>
      dplyr::group_by(type) |>
      dplyr::summarize(loaded = paste0(sum(loaded), '/', dplyr::n()),
                       size = sum(size, na.rm=TRUE),
                       modified = my_max(modified),
                       groups = paste(unique(group), collapse = ', ')) |>
      dplyr::mutate(type = as.character(type))

  # report the number of objects after summarization
  if(!quiet)
  {
    n_out = nrow(files_df)
    msg_level = paste0(level, ifelse(n_out > 0, 's', ''))
    message(paste(n_out, msg_level, 'in', .db$get_swat_dir()))
  }

  # return as tibble with specified column order
  return( dplyr::tibble(rswat_order_columns(files_df, nm_print)) )
}

