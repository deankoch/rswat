
#' Write parameters to SWAT+ config file(s)
#'
#' Returns a tibble summarizing modifications to SWAT+ configuration file(s) corresponding
#' to the changes found in the data frame(s) `new_df`. When `overwrite=TRUE`, these changes
#' are written to disk.
#'
#' `new_df` should be a data frame returned by `rswat_open`, or a list of them. Be aware of
#' the attributes in these objects with names beginning with 'rswat_'. If some operation
#' drops the attributes, `rswat_write` will not be able to locate the source file.
#'
#' Set `fast=TRUE` to omit most of the summary tibble output and return only the names of the
#' files to modify. If `overwrite=TRUE` and `fast=TRUE` the function also renders tables using
#' `data.table::fwrite` (instead of `print.data.frame`). This is much faster with large files,
#' but results in configuration files that are less readable to humans (columns are not aligned).
#'
#' `fast=TRUE` mode is mandatory for 'weather' type input data files ('pcp1.pcp' etc).
#'
#' Argument `refresh=FALSE` disables automatic reloading of files. This is useful for speeding
#' performance on bulk overwrites of large files (like weather data) but not recommended for
#' interactive sessions.
#'
#' @param new_df data frame or list, the SWAT+ table(s) to write
#' @param overwrite logical, by default FALSE. Set to TRUE to write changes to disk
#' @param fast logical, enables fixed delimiters for faster writing
#' @param quiet logical, suppresses console messages
#' @param refresh logical, if `TRUE` the function reloads the file after modifying it
#' @param .db rswat reference object, for internal use
#'
#' @return a tibble listing changes to make in the affected SWAT+ files
#' @export
rswat_write = function(new_df,
                       overwrite = FALSE,
                       fast = FALSE,
                       quiet = FALSE,
                       refresh = TRUE,
                       .db = .rswat_db) {

  modify_tense = ifelse(overwrite, 'modified', 'to modify')

  # handle list input with recursive call
  if( !is.data.frame(new_df) )
  {
    if( !is.list(new_df) ) stop('first argument must be a data frame or list of them')
    write_result = lapply(new_df, \(df_i) rswat_write(df_i,
                                                      overwrite = overwrite,
                                                      fast = fast,
                                                      quiet = TRUE,
                                                      refresh = refresh,
                                                      .db=.rswat_db))

    # list of changes in the file
    df_changes = do.call(rbind, write_result) |> dplyr::arrange('file')

    # print info about the pending changes
    is_changed = nrow(df_changes) > 0
    if(is_changed) {

      n_file = dplyr::n_distinct(df_changes[['file']])
      msg_change = paste(n_file, 'file(s)')

      # in non-fast mode report on field(s) modified
      msg_extra = paste(nrow(df_changes), 'field(s)', modify_tense)
      if( !fast ) msg_change = msg_extra |> paste('in', msg_change)

    } else { msg_change = 'no changes to write' }

    if(!quiet) message(msg_change)
    return( dplyr::tibble(df_changes) )
  }

  # check for and copy two required attributes
  fn = attr(new_df, 'rswat_fname')
  table_num = attr(new_df, 'rswat_table_num')
  if( length(c(fn, table_num)) < 2L ) stop('input new_df missing required rswat attributes')

  # copy destination file info
  swat_dir = .db$get_swat_dir()
  fn = attr(new_df, 'rswat_fname')
  table_num = attr(new_df, 'rswat_table_num')
  fn_info = .db$get_cio_df(f=fn)

  # stop when new_df is an output table
  err_output = 'rswat cannot write to output files'
  if( fn_info[['type']] == 'output' ) stop(err_output)

  # formatting and error checking
  new_df = rswat_prewrite(new_df, quiet=quiet, .db=.db)

  # by default assume the file will be changed
  is_changed = TRUE
  txt_replace = if(fast) {

    # render table as line-by-line plain text (fast method)
    data.table::fwrite(new_df, sep=' ', eol='\n', quote=FALSE) |> utils::capture.output()

  } else {

    # render using base print for readable spacing (slow method)
    txt_replace = print.data.frame(new_df,
                                   row.names=FALSE,
                                   width=.rswat_gv_precision('n_line'),
                                   max=.rswat_gv_precision('n_all')) |> utils::capture.output()

    # debugging: fix double-escaped backslashes
    txt_replace = gsub('\\\\', '\\', txt_replace, fixed=TRUE)
  }

  # copy the existing table data and lines info data frame for this file
  txt_old = .db[['txt']][[fn]]
  line_df_old = .db$get_line_df(f=fn, check_dir=FALSE)

  # find the line numbers corresponding to the replacement
  is_replaced = line_df_old[['table']] %in% table_num
  ln_start = min(line_df_old[['line_num']][is_replaced], na.rm=TRUE)
  ln_end = max(line_df_old[['line_num']][is_replaced], na.rm=TRUE)
  ln_replaced = seq(ln_start, ln_end)

  # drop data frame headers when they are not present in the file
  has_headers = any(line_df_old[['header']][is_replaced])
  if( !has_headers ) txt_replace = txt_replace[-1L]

  # hack to deal with 'weather-wgn.cli' case
  if( ( fn == 'weather-wgn.cli' ) & ( table_num == max(line_df_old[['table']]) ) )
  {
    # these table rows are spread out over the file, so we splice new lines with old
    txt_to_change = txt_replace
    txt_replace = txt_old[ln_table]
    txt_replace[ln_replaced - ln_start + 1] = txt_to_change
  }

  # hack to deal with weather inputs case (where only config rows are hashed/cached)
  if( fn_info[['type']] == 'weather' ) {

    txt_old = readLines(fn_info[['path']])
    fast = TRUE
  }

  # build the new line-by-line strings
  n_line_old = length(txt_old)
  txt_pre = txt_old[seq(n_line_old) < ln_start]
  txt_post = txt_old[seq(n_line_old) > ln_end]
  txt_write = c(txt_pre, txt_replace, txt_post)

  # summary skipped in fast mode but report files to modify
  if(fast) { changes_df = data.frame(file=fn) } else {

    # parse the result with rswat, extract changes, tidy up clutter and show existing vs replacement
    nm_match = c('line_num', 'field_num')
    changes_df = txt_write |>
      rswat_scan_txt(f=fn, type='config') |>
      rswat_ftable_txt() |>
      dplyr::anti_join(line_df_old, by=c('string', nm_match)) |>
      dplyr::mutate('replacement'=string) |>
      dplyr::select(all_of(c('replacement', nm_match))) |>
      dplyr::right_join(line_df_old, by=nm_match) |>
      dplyr::filter(!is.na(replacement)) |>
      dplyr::rename('value' = 'string') |>
      dplyr::select('file', 'name', 'table', 'line_num', 'field_num', 'value', 'replacement') |>
      dplyr::arrange('field_num')

    # prepare console messages, set flag to indicate pending changes
    msg_change = paste(nrow(changes_df), 'field(s)', modify_tense, 'in', fn)
    is_changed = nrow(changes_df) > 0
    if(!is_changed) message('no changes to write')
    if( !quiet ) message(msg_change)
  }

  # write the changes to disk in overwrite mode
  if( overwrite & is_changed )
  {
    # path to overwrite
    path_write = file.path(swat_dir, fn)
    if( !quiet ) message( paste('writing changes to', path_write, '\n') )

    # write the file and reload it
    writeLines(txt_write, path_write)
    if(refresh) rswat_open(fn, quiet=quiet, output=FALSE, .db=.db)
  }

  if(quiet) return(invisible(dplyr::tibble(changes_df)))
  return(dplyr::tibble(changes_df))
}

#' Prepare a data frame for writing to a SWAT+ file
#'
#' Replace an existing SWAT+ parameter table with a new, possibly modified version,
#'
#' Generate a formatted version of the input data frame, ready to be printed to a
#' file. For internal use by `rswat_write`.
#'
#' The function replaces missing columns, drops unnecessary ones, coerces mismatched
#' classes, and converts all numeric columns to character with the same precision level
#' as found in the file. Most related warnings and messages can be disabled with
#' `quiet=TRUE`.
#'
#' Logical columns are converted to character, as 'y'/'n'.
#'
#' @param new_df data frame, the new SWAT+ parameter table
#' @param quiet logical, whether to warn of possible problems with input
#' @param .db rswat reference object, for internal use
#'
#' @return data frame, a modified version of `new_df` ready to print
#' @export
rswat_prewrite = function(new_df, quiet=FALSE, .db=.rswat_db)
{
  # check for and copy two required attributes
  swat_dir = attr(new_df, 'rswat_path')
  fn = attr(new_df, 'rswat_fname')
  table_num = attr(new_df, 'rswat_table_num')
  if( length(c(fn, table_num)) < 2L ) stop('input new_df missing required rswat attributes')

  # get data frame of (old) parameter data on disk
  .db$open_config_file(fn, output=FALSE)
  old_df = .db[['stor_df']][[fn]][[table_num]]
  if( is.null(old_df) ) stop('file creation mode not yet implemented')

  # warn if changing the number of rows
  nrow_change = nrow(old_df) != nrow(new_df)
  msg_change = paste('input has', nrow(new_df), 'row(s). Existing table has', nrow(old_df))
  if( !quiet & nrow_change ) warning(msg_change)

  # warn of unrecognized columns and remove them
  new_nm = names(new_df)
  old_nm = names(old_df)
  is_known = new_nm %in% old_nm
  nm_extra = paste(names(new_df)[!is_known], collapse=', ')
  if( any(!is_known) & !quiet ) warning( paste('ignoring unrecognized column(s):', nm_extra) )
  new_df = new_df[is_known]

  # deal with missing columns
  is_missing = !(old_nm %in% new_nm)
  if( any(is_missing) ) {

    # if the number of rows is changing we require all columns to be supplied by the user
    err_missing = paste('new_df should have columns', paste(old_nm[is_missing], collapse=', '))
    if(nrow_change) stop(err_missing)

    # otherwise we just use the existing ones in the file
    new_df = rswat_order_columns(cbind(new_df, old_df), old_nm)
  }

  # drop the any units attributes numeric columns in new data frame
  has_units = sapply(new_df, \(x) 'units' %in% class(x))
  if( any(has_units) ) {

    msg_units = paste('dropping units from column(s):', paste(collapse = ', '))
    if( !quiet ) warning( msg_units )
    new_df[has_units] = apply(new_df[has_units], 2, as.numeric)
  }

  # get classes of existing fields and replacements
  new_class = sapply(new_df, \(x) head(class(x), 1L))
  old_class = sapply(old_df, \(x) head(class(x), 1L))

  # deal with all-NA columns (defaulting to `NA_character_`) by trusting user's replacement class
  all_na = sapply(old_df, \(x) all(is.na(x)))
  if( any(all_na) ) old_class[all_na] = new_class[all_na]

  # coerce columns to the proper class
  is_coerced = new_class != old_class
  if( any(is_coerced) )
  {
    expected_class = paste0('(to ', old_class[is_coerced], ')')
    info_coerce_nm = Map(\(nm, cl) paste(nm, cl), old_nm[is_coerced], expected_class)
    warn_coerce = paste0('coercing column(s) ', paste(info_coerce_nm, collapse=', '))
    if( !quiet ) warning(warn_coerce)
    new_df[is_coerced] = Map(\(v, cl) as(v, cl), new_df[is_coerced], old_class[is_coerced])
  }

  # format numeric as character with specified precision level
  is_numeric = old_class == 'numeric'
  if( any(is_numeric) )
  {
    # set precision and assign default when unknown
    prec_lu = .db$get_line_df(f=fn) |>
      dplyr::filter(header) |>
      dplyr::filter(table==table_num) |>
      dplyr::filter(class=='numeric') |>
      dplyr::mutate(n_prec=replace(n_prec, is.na(n_prec), .rswat_gv_precision('n_small'))) |>
      dplyr::select(name, n_prec)

    # make sure we are matching name to precision in same order as new_df
    n_prec = prec_lu[['n_prec']][ match(names(new_df)[is_numeric], prec_lu[['name']]) ]

    # replace NAs with this placeholder
    na_placeholder = '  '

    # deal with weather input files which have no headers and use a different placeholder
    if( any(is.na(n_prec)) ) {

      n_prec = rep(.rswat_gv_precision('n_small'), sum(is_numeric))
      na_placeholder = .rswat_gv_weather_NA_val()
    }

    # overwrite numeric columns with character
    new_df[is_numeric] = Map(\(v, n) {

      char_out = format(v, digits=.rswat_gv_precision('digits'), nsmall=n)
      replace(char_out, is.na(v) | is.na(char_out), na_placeholder)

    }, v=new_df[is_numeric], n=n_prec) |> as.data.frame()
  }

  # convert logical columns to 'y', 'n'
  is_logical = old_class == 'logical'
  if( any(is_logical) ) new_df[is_logical] = new_df[is_logical] |>
    sapply(\(x) c('n', 'y')[ 1 + as.integer(x) ] )

  # replace any remaining NAs with two spaces
  is_na = sapply(new_df, anyNA)
  new_df[is_na] = new_df[is_na] |> replace(is.na(new_df[is_na]), '  ')

  # return the pre-processed data frame with attributes copied back
  attr(new_df, 'rswat_path') = swat_dir
  attr(new_df, 'rswat_fname') = fn
  attr(new_df, 'rswat_table_num') = table_num
  return(new_df)
}


#' Save a copy of a SWAT+ project, or a subset of its files
#'
#' Makes a copy of some or all of the contents of the currently loaded SWAT+ project
#' directory. Call the function without arguments to create a new backup of all SWAT+
#' files known to `rswat` in a sub-directory.
#'
#' Specify other paths for the backup using `dest`. Set `zip=TRUE` to compress
#' to a zip file instead, at path `dest`. Set `overwrite=TRUE` to allow `rswat`
#' to make changes to `dest` on disk, and set `wipe=TRUE` to first delete any existing
#' file(s) found at `dest`.
#'
#' By default, the output directory or file is written to the currently loaded SWAT+ project
#' directory and is named '.rswat_backup_\\<DATE\\>', where \\<DATE\\> is the current date-time
#' as reported by `Sys.time()`, in snake case. If a zip file is being written, the extension
#' '.zip' is appended automatically (if it isn't there already). Note that on some operating
#' systems the prefix '.' hides the file unless it is referenced specifically.
#'
#' Subsets of the SWAT+ project can be be specified by `include` and `exclude`, as
#' described in `?rswat_files`. When they are both `NULL`, the function copies all known
#' SWAT+ files. Note that this includes 'weather' and 'output' type files, which can be
#' large in size but which also tend to compress well.
#'
#' Files unknown to `rswat` because they are not listed in 'file.cio' are excluded
#' automatically (regardless of `include` and `exclude`), as are existing `rswat`
#' backups and sub-directories.
#'
#' @param dest character, path to the desired output file or folder
#' @param include character vector, a set of file, group, or type names to include
#' @param exclude character vector, a set of file, group, or type names to exclude
#' @param zip logical, indicates to write a zip instead of a folder
#' @param overwrite logical, enables modifying or deleting contents of dest
#' @param wipe  logical, deletes the file or folder dest before writing to it
#' @param quiet logical, suppresses console output
#' @param .db rswat reference object, for internal use
#'
#' @return character vector, the path(s) to the file(s) written
#' @export
#'
rswat_backup = function(dest = NULL,
                        include = NULL,
                        exclude = NULL,
                        zip = FALSE,
                        overwrite = FALSE,
                        wipe = FALSE,
                        quiet = FALSE,
                        .db = .rswat_db) {

  # get current project directory and set defaults
  date_time_string = format(Sys.time(), '%Y_%m_%d_%Z_%H%M_%S') |> tolower()
  swat_dir = .db$get_swat_dir()
  if( is.null(exclude) ) dest = file.path(swat_dir, paste0('.rswat_backup_', date_time_string))

  # make a list of known SWAT+ files in current project and omit backups
  file_df = rswat_files(known = TRUE,
                        include = include,
                        exclude = exclude,
                        refresh = TRUE,
                        quiet = TRUE,
                        .db = .db) |> dplyr::filter(type != 'backup')

  # handle no files case
  n_file = nrow(file_df)
  if(n_file == 0L) {

    # different advice depending on include/exclude
    msg_advice = ifelse(!is.null(exclude),
                        yes = '. Try setting exclude=NULL',
                        no = ifelse(!is.null(include),
                                    yes = '. Try setting include=NULL',
                                    no = '. Did you mean to import?'))

    stop(paste0('no files to copy', msg_advice))
    return(character(0L))
  }

  # clean up destination path and add zip file extension as needed
  dest = dest |>
    normalizePath(winslash='/', mustWork=FALSE) |>
    paste0(ifelse(zip & !endsWith(dest, '.zip'), '.zip', ''))

  # check if the destination exists already
  if( dest == swat_dir ) stop('dest path is the currently loaded SWAT+ directory')
  dest_exists = ifelse(zip, file.exists(dest), dir.exists(dest))
  if(dest_exists) {

    # stop here unless user has requested overwrite
    if(!overwrite) {

      msg_exists = paste(ifelse(zip, 'file', 'directory'), basename(dest), 'exists already')
      if(!quiet) message(paste(msg_exists, '(set overwrite=TRUE to copy anyway)'))
      return(character(0L))
    }
  }

  # copy/zip, reporting progress
  if(!quiet) message(paste(ifelse(zip, 'zipping', 'copying'),
                           n_file, 'file(s) to',
                           paste0(dest, ifelse(zip, ' ...', '/'))))

  # remove the existing file(s) on request
  if(wipe & dest_exists) {

    if(!quiet) message(paste('deleting existing', ifelse(zip, 'zip file', 'directory')))
    unlink(dest, recursive=TRUE)
  }

  # compress all files using base::zip with default settings
  if(zip) {

    # rswat_zip is just utils::zip with stdout discarded so as to not spam the console
    zip_value = rswat_zip(dest, file.path(swat_dir, file_df[['file']]), quiet=TRUE)
    copy_success = file.exists(dest)
    file_written = dest

    # find the compressed size
    msg_size = file.info(file_written)[['size']] |>
      units::set_units(bytes) |> units::set_units(Megabytes) |>
      round(2L) |> as.character() |> paste('Mb')

  } else {

    # make the directory then loop over files, copying each one to dest directory
    if(!dir.exists(dest)) dir.create(dest)
    msg_progress = file_df[['file']] |> rwat_progress()
    copy_success = logical(length(msg_progress)) |> stats::setNames(file_df[['file']])
    for(f in file_df[['file']]) {

      if(!quiet) msg_progress[f] |> cat()
      src_path = file.path(swat_dir, f)
      copy_success[f] = file.copy(src_path, file.path(dest, f), overwrite=TRUE, copy.date=TRUE)
    }

    # make a a list of files written and their total size
    if(!quiet) cat('\n')
    file_written = file.path(dest, file_df[['file']][copy_success])
    msg_size = sum(file_df[['size']][copy_success]) |> units::set_units(Megabytes) |>
      round(3L) |> as.character() |> paste('Mb')
  }

  # report any errors then report on what was written
  msg_fail = paste('failed to write', paste(file_df[['file']][!copy_success], collapse=', '))
  if( any(!copy_success) ) { warning(msg_fail) } else {

    if(!quiet) message(paste('done. Wrote', msg_size))
  }

  # return (but don't print) the file paths written
  return( invisible(file_written) )
}


#' Restore a backup of a SWAT+ project, or a subset of its files
#'
#' `backup` must point to either a directory with SWAT+ project files, or
#' else a zip file containing them. If `backup` is `NULL`, the function returns a tibble
#' of information on available backups found in the current SWAT+ project directory.
#'
#' Users must toggle `overwrite=TRUE` to enable copying. When it is `FALSE` (the default),
#' the function returns a data frame of files that would be created/modified, but makes
#' no changes to the files on disk.
#'
#' The call to `utils::unzip` uses `junkpaths=TRUE`, so any directory structure in the
#' zip file will be collapsed (probably a bad thing when unintended). It is recommended
#' to only load zip files created by `rswat` (see `?rswat_export`)
#'
#' @param backup character, path to the
#' @param dest character, path to the desired output file or folder
#' @param overwrite logical, enables modifying or deleting contents destination directory
#' @param quiet logical, suppresses console output
#' @param .db rswat reference object, for internal use
#'
#' @return character vector, the path(s) to the file(s) written
#' @export
rswat_restore = function(backup = NULL,
                         dest = NULL,
                         overwrite = FALSE,
                         quiet = FALSE,
                         .db = .rswat_db) {

  # helper for below, lists contents of a backup directory or zip (p=path, g=group)
  open_backup = function(p, g=NULL) {

    # set group based on file name
    if( is.null(g) ) g =  c('dir', 'zip')[ 1L + as.integer(endsWith(p, '.zip')) ]
    msg_fail = paste('there was a problem with the path', p)
    if( is.na(g) ) stop(msg_fail)
    if( g == 'dir' ) return( list.files(p) )
    if( g == 'zip' ) {

      # ignore paths and directories in zip
      zip_paths = utils::unzip(p, list=TRUE)[['Name']]
      return( basename(zip_paths[!endsWith(zip_paths, '/')]) )
    }

    stop(msg_fail)
  }

  # set default destination to currently loaded SWAT+ project
  if( is.null(dest) ) dest = .db$get_swat_dir()
  if( is.na(dest) ) stop('SWAT+ directory not assigned. Set it with rswat(...) or else provide dest')

  # normalize and validate destination path
  dest = dest |> normalizePath(winslash='/', mustWork=FALSE)
  if( !file.exists(dest) ) {

    # create missing directory in overwrite mode
    if( !overwrite ) stop(paste('directory', dest, 'not found. Set overwrite=TRUE to create it'))
    if( !quiet ) message( paste('creating directory', dest) )
    dir.create(dest, recursive=TRUE)
  }

  # get SWAT+ file info on dest as data frame
  existing_files = rswat_scan_dir(dest, f=list.files(dest))

  # if backup path is not given, return a tibble listing backups in currently loaded project folder
  if( is.null(backup) ) {

    # list files in each backup
    backup_df = rswat_files(quiet=TRUE, .db=.db) |> dplyr::filter(type=='backup')
    if( nrow(backup_df) > 0L ) {

      # function open_backup is defined above)
      backup_files = Map(open_backup,
                         p = file.path(dest, backup_df[['file']]),
                         g = backup_df[['group']])

      # add some summary info and remove clutter for output
      backup_df = backup_df |>
        dplyr::mutate('n_file' = sapply(backup_files, length)) |>
        dplyr::mutate('files' = sapply(backup_files, \(v) paste(v, collapse=', '))) |>
        dplyr::select(c('file', 'group', 'n_file', 'files', 'size', 'modified')) |>
        dplyr::arrange('modified')

      if(!quiet) message( paste('found', nrow(backup_df), 'backups in', dest) )
      return( dplyr::tibble(backup_df) )
    }

    # no backups case returns empty tibble invisibly
    if(!quiet) message( paste('no backup files found in', dest) )
    return( invisible(dplyr::tibble()) )
  }

  # validate two possible paths and pick the first one that works
  path_test = c(backup, file.path(dest, backup)) |> normalizePath(winslash='/', mustWork=FALSE)
  path_exists = file.exists(path_test)
  if( !any(path_exists) ) stop( paste('invalid path', backup) )
  backup_norm = path_test[ which(path_exists)[1L] ]

  # get files list from backup and handle empty case
  backup_files = open_backup(backup_norm)
  if( length(backup_files) == 0 ) {

    # this would indicate either an empty directory/zip or a bug with open_backup
    warning( paste('rswat did not find any files in', backup_norm) )
    return( invisible(dplyr::tibble()) )
  }

  # compare with existing files list to identify files to overwrite
  existing_files = existing_files |>
    dplyr::mutate(add = FALSE) |>
    dplyr::mutate(overwrite = file %in% backup_files)

  # identify any new files introduced from this backup
  new_filenames = backup_files[ !( backup_files %in% existing_files[['file']] ) ]
  new_files = rswat_scan_dir(dest, f=new_filenames) |>
    dplyr::mutate(add = TRUE) |>
    dplyr::mutate(overwrite = FALSE)

  # merge the two lists, remove clutter
  restore_summary = rbind(existing_files, new_files) |>
    dplyr::filter(overwrite|add) |>
    dplyr::select(c('file', 'overwrite', 'add')) |>
    dplyr::mutate(completed = FALSE)

  # count files being changed
  n_add = restore_summary[['add']] |> sum()
  n_over = restore_summary[['overwrite']] |> sum()
  if(!quiet & !overwrite) {

    # build and print a message about files to be changed
    msg_over = paste0('overwrite ', n_over, ' file(s)', ifelse(n_add > 0, ' and ', ''))
    msg_add = paste0('add ', n_add, ifelse(n_over > 0, '', ' files(s)'))
    paste0('This will ', ifelse(n_over > 0, msg_over, ''), ifelse(n_add > 0, msg_add, '')) |>
      paste0(' in ', dest) |>
      paste0('\nSet overwrite=TRUE to write changes to disk') |>
      message()
  }

  # write changes or print a message
  if( overwrite ) {

    # build a message about the job
    n_file = n_add + n_over
    is_zip = endsWith(backup_norm, '.zip')
    msg_extract = paste(ifelse(is_zip, 'unzipping', 'copying'), n_file, 'file(s)')
    if(!quiet) message(msg_extract)

    # copy/zip
    fn = restore_summary[['file']]
    if(is_zip)
    {
      # return value is the file path copied
      copied = utils::unzip(backup_norm, exdir=dest, overwrite=TRUE, junkpaths=TRUE)
      is_copied = fn %in% basename(copied)

    } else {

      # return value is logical indicating success
      is_copied = file.copy(from = file.path(backup_norm, fn),
                            to = file.path(dest, fn),
                            overwrite = TRUE)
    }

    # copy status gets reported in output and column names change to past tense
    restore_summary[['completed']][is_copied] = TRUE
    restore_summary = restore_summary |> dplyr::rename('overwritten'='overwrite', 'added'='add')
  }

  return(dplyr::tibble(restore_summary))
}
