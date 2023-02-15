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
                         dest = .db$get_swat_dir(),
                         overwrite = FALSE,
                         quiet = FALSE,
                         .db = .rswat_db) {


  # helper for below, lists contents of a backup directory or zip (p=path, g=group)
  open_backup = function(p, g=NULL) {

    if( is.null(g) ) g =  c('dir', 'zip')[ 1L + as.integer(endsWith(p, '.zip')) ]
    msg_fail = paste('there was a problem with the path', p)
    if( is.na(g) ) stop(msg_fail)
    if( g == 'dir' ) return( list.files(p) )
    if( g == 'zip' ) return( unzip(p, list=TRUE)[['Name']] )
    stop(msg_fail)
  }

  # normalize and validate destination path
  dest = dest |> normalizePath(winslash='/')

  # get SWAT+ file info on dest as data frame
  existing_files = rswat_scan_dir(dest, f=list.files(dest))

  # if backup path is not given, return a tibble listing backups in current directory
  if( is.null(backup) )
  {
    # list files in each backup
    backup_df = rswat_files(quiet=TRUE, .db=.db) |> dplyr::filter(type=='backup')
    if( nrow(backup_df) > 0L )
    {
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

  # get files list from backup and compare with existing files list
  backup_files = open_backup(backup_norm)
  existing_files = existing_files |>
    dplyr::mutate(add = FALSE) |>
    dplyr::mutate(overwrite = file %in% backup_files)

  # identify any new files introduced from this backup
  new_filenames = backup_files[ !( backup_files %in% existing_files[['file']] ) ]
  new_files = rswat_scan_dir(dest, f=new_filenames) |>
    dplyr::mutate(add = TRUE) |>
    dplyr::mutate(overwrite = FALSE)

  # merge the two lists and remove clutter
  restore_summary = rbind(existing_files, new_files) |>
    dplyr::filter(overwrite|add) |>
    dplyr::select(c('file', 'type', 'overwrite', 'add')) |>
    dplyr::mutate(completed = FALSE)

  # count files being changed
  n_add = restore_summary[['add']] |> sum()
  n_over = restore_summary[['overwrite']] |> sum()
  msg_backup = paste('this will add', n_add, 'new file(s) and overwrite', n_over, 'in', dest)
  if(!quiet) message(msg_backup)

  # write changes or print a message
  if( !overwrite ) { if(!quiet) message('set overwrite=TRUE to write changes') } else {

    # build a message about the job
    n_file = n_add + n_over
    is_zip = endsWith(backup_norm, '.zip')
    msg_extract = paste(ifelse(is_zip, 'unzipping', 'copying'), n_file, 'file(s) to', dest)
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

    # copy status gets reported in output
    restore_summary[['completed']][is_copied] = TRUE
  }

  return(restore_summary)
}
