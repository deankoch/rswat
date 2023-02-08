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
  old_output_files = rswat_files(what=NULL, quiet=TRUE, .db=.db)

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
  .db$refresh_cio_df()
  new_output_files = rswat_files(what=NULL, quiet=TRUE, .db=.db)
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

  # make a list of known SWAT+ files in current project
  file_df = rswat_files(known = TRUE,
                        include = include,
                        exclude = exclude,
                        refresh = TRUE,
                        quiet = TRUE,
                        .db = .db)

  # omit existing backups and files not found on disk
  file_df = file_df[file_df[['exists']] & (file_df[['type']] != 'backup'), , drop=FALSE]
  n_file = nrow(file_df)

  # handle no files case
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
    zip_value = rswat_zip(dest, file_df[['path']], quiet=quiet)
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
    for(p in file_df[['path']]) {

      f = basename(p)
      if(!quiet) msg_progress[f] |> cat()
      copy_success[f] = file.copy(p, file.path(dest, f), overwrite=TRUE, copy.date=TRUE)
    }

    # make a a list of files written and their total size
    if(!quiet) cat('\n')
    file_written = file.path(dest, file_df[['file']][copy_success])
    msg_size = sum(file_df[['size']][copy_success]) |> units::set_units(Megabytes) |>
      round(2L) |> as.character() |> paste('Mb')
  }

  # report any errors then report on what was written
  msg_fail = paste('failed to write', paste(file_df[['file']][!copy_success], collapse=', '))
  if( any(!copy_success) ) { warning(msg_fail) } else {

    if(!quiet) message(paste('done. Wrote', msg_size))
  }

  # return but don't show the file paths written
  return( invisible(file_written) )
}


#' Restore a backup of a SWAT+ project, or a subset of its files
#'
#' `backup` must point to either a directory with SWAT+ project files, or
#' else a zip file containing them. If `backup` is `NULL`, the function looks in the
#' currently loaded SWAT+ project directory and selects the most recently modified
#' file or directory with name starting with '.rswat_backup'.
#'
#' Users must toggle `overwrite=TRUE` to enable copying. When it is `FALSE` (the default),
#' the function returns a data frame of files that would be created/modified, but makes
#' no changes to the files on disk.
#'
#' When overwrite is `TRUE`, users can toggle `wipe=TRUE` to first erase all known SWAT+ files
#' in the currently loaded project before copying over the backups. Exceptions which are never
#' deleted include: sub-directories, existing `rswat` backups, and unknown config files (ie
#' those not listed in 'file.cio').
#'
#' `include` and `exclude` define a subset of files in the current project directory
#' (see `?rswat_files`) to restore from the backup. The function will attempt to overwrite
#' them with the files of the same name in `backup`, ignoring files not found in `backup`.
#' The function ignores files in `backup` that are not identified by `include` (eg config
#' files in the backup that are missing from the current directory), unless `include=NULL`.
#'
#' `include=NULL` (the default) indicates to restore everything in `backup`, with the
#' exception of any file names mentioned specifically in `exclude`.
#'
#' The call to `utils::unzip` uses `junkpaths=TRUE`, so any directory structure in the
#' zip file will be collapsed (probably a bad thing). It is recommended to only load
#' zip files created by `rswat` (see `?rswat_export`)
#'
#' @param backup character, path to the desired output file or folder
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
rswat_restore = function(backup = NULL,
                         include = NULL,
                         exclude = NULL,
                         overwrite = FALSE,
                         wipe = FALSE,
                         quiet = FALSE,
                         .db = .rswat_db) {

  # copy current SWAT+ project directory and known file info
  swat_dir = .db$get_swat_dir()
  existing_files = rswat_files(what=c('file', 'exists')) |> dplyr::filter(exists)
  file_df = rswat_files(known = TRUE,
                        include = include,
                        exclude = c(exclude, 'backup'),
                        refresh = TRUE,
                        quiet = TRUE,
                        .db = .db)

  # omit existing backups and files not found on disk
  file_df = file_df[file_df[['exists']] & (file_df[['type']] != 'backup'), , drop=FALSE]
  n_file = nrow(file_df)

  # if backup path is not given, look for newest backup in current directory
  if( is.null(backup) )
  {
    existing_backup = rswat_files('backup', .db=.db) |> dplyr::arrange(dplyr::desc(modified))
    if( nrow(existing_backup) == 0L ) stop(paste('no backup files found in', swat_dir))
    backup = existing_backup[['path']][1L]
    msg_modified = paste('last modified', existing_backup[['modified']][1L])
    if( !quiet ) message(paste0('selecting backup = "', basename(backup), '"\n', msg_modified))
  }

  # rswat_scan_dir returns file info for directories, or empty data frame for zip
  backup_df = rswat_scan_dir(backup, f=list.files(backup))
  if( endsWith(backup, '.zip') & ( nrow(backup_df) == 0L ) ) {

    # get a list of files from the backup zip
    unzip_df = utils::unzip(backup, list=TRUE)[c('Name', 'Date')] |>
      dplyr::rename(Name='file', Date='modified') |>
      dplyr::mutate(file = basename(file))



    # exclude backups
    backup_df = rswat_scan_dir(backup, f=unzip_df[['file']]) |>
      dplyr::mutate(modified = unzip_df[['modified']]) |>
      dplyr::filter(type != 'backup')
  }

  # identify files to copy
  is_requested = backup_df[['file']] %in% file_df_test[['file']]
  if( is_requested) is_requested = rep(TRUE, )
  files_requested =
  files_requested


  existing_files

  backup_df[['file']]


  # handle no files case
  if(n_file == 0L) {

  }



  is_zip = FALSE
  n_files = 2

  if( !quiet ) message(paste(ifelse(overwrite, 'selected', 'copying'),
                             n_files, 'SWAT+ file(s)',
                             'from backup', ifelse(is_zip, 'file:', 'directory:'),
                             backup))

  #utils::unzip(backup, , junkpaths=TRUE)

}




# rswat_copy = function(import = NULL,
#                       export = NULL,
#                       include = NULL,
#                       exclude = NULL,
#                       as_zip = FALSE,
#                       overwrite = FALSE,
#                       wipe = FALSE,
#                       quiet = FALSE,
#                       .db = .rswat_db) {
#
#   # check for invalid input
#   is_import = !is.null(import)
#   is_export = !is.null(export)
#   if( is_export & is_import ) stop('Specify either import or export (not both)')
#
#   # default behaviour is to export
#   swat_dir = .db$get_swat_dir()
#   if( !is_export & !is_import ) {
#
#     # suggest a destination file/directory
#     is_export = TRUE
#     export = swat_dir |>
#       file.path(paste0('rswat_backup_', format(Sys.time(), '%Y_%m_%d_%Z_%H%M_%S')))
#   }
#
#   # make a list of known SWAT+ files in current project
#   file_df = rswat_files(known = TRUE,
#                         include = include,
#                         exclude = exclude,
#                         refresh = TRUE,
#                         quiet = TRUE,
#                         .db = .db)
#
#   # omit existing backups and files not found on disk
#   file_df = file_df[file_df[['exists']] & (file_df[['type']] != 'backup'), , drop=FALSE]
#   n_file = nrow(file_df)
#
#   # handle export requests
#   if(is_export) {
#
#     # handle no files case
#     if( n_file == 0L ) {
#
#       # different advice depending on include/exclude
#       msg_advice = ifelse(!is.null(exclude),
#                           yes = '. Try setting exclude=NULL',
#                           no = ifelse(!is.null(include),
#                                       yes = '. Try setting include=NULL',
#                                       no = '. Did you mean to import?'))
#
#       stop(paste0('no files to copy', msg_advice))
#       return(NULL)
#     }
#
#     # clean up destination path and add zip file extension as needed
#     export = export |>
#       normalizePath(winslash='/', mustWork=FALSE) |>
#       paste0(ifelse(as_zip & !endsWith(export, '.zip'), '.zip', ''))
#
#     # check if the destination exists already
#     if( export == swat_dir ) stop('export path is the currently loaded SWAT+ directory')
#     export_exists = ifelse(as_zip, file.exists(export), dir.exists(export))
#     if(export_exists) {
#
#       # stop here unless user has requested overwrite
#       if(!overwrite) {
#
#         msg_exists = paste(ifelse(as_zip, 'file', 'directory'), basename(export), 'exists already')
#         message(paste(msg_exists, '(set overwrite=TRUE to copy anyway)'))
#         return(NULL)
#       }
#     }
#
#     # copy/zip, reporting progress
#     if(!quiet) message(paste(ifelse(as_zip, 'zipping', 'copying'),
#                              n_file, 'file(s) to',
#                              paste0(export, ifelse(as_zip, ' ...', '/'))))
#
#     # remove the existing file(s) on request
#     if(wipe & export_exists) {
#
#       if(!quiet) message(paste('deleting existing', ifelse(as_zip, 'zip file', 'directory')))
#       unlink(export, recursive=TRUE)
#     }
#
#     # compress all files in zip
#     if(as_zip) {
#
#       # TODO: make a quiet version of this function by setting stdout=FALSE in system2 call...
#       zip_value = rswat_zip(export, file_df[['path']], quiet=TRUE)
#       copy_success = file.exists(export)
#       file_written = export
#
#     } else {
#
#       # make the directory then loop over files, copying each one to export directory
#       if(!dir.exists(export)) dir.create(export)
#       msg_progress = file_df[['file']] |> rwat_progress()
#       copy_success = logical(length(msg_progress)) |> stats::setNames(file_df[['file']])
#       for(p in file_df[['path']]) {
#
#         f = basename(p)
#         if(!quiet) msg_progress[f] |> cat()
#         copy_success[f] = file.copy(p, file.path(export, f), overwrite=TRUE, copy.date=TRUE)
#       }
#
#       if(!quiet) cat('\n')
#       file_written = file.path(export, file_df[['file']][copy_success])
#     }
#
#     # report any errors then report number of files written
#     msg_fail = paste('failed to write', paste(file_df[['file']][!copy_success], collapse=', '))
#     if( any(!copy_success) ) { warning(msg_fail) } else { message('done')}
#     return( file_written )
#   }
#
#
#
#
#
#
#   stop('not implemented yet')
#
#
#
#
#
#
#
#
#   #
#   # # set default source and destination paths as needed
#   # to.default = file.path(textio, paste0('_rswat_backup_', basename(tempfile()) ) )
#   # if( is.null(from) ) from = textio
#   # if( is.null(to) )
#   # {
#   #   # default destination is either textio or a subdirectory, depending on `from`
#   #   if( from == textio ) to = to.default
#   #   if( from != textio ) to = textio
#   # }
#   #
#   # # default files list includes everything listed in 'file.cio' (and the file itself)
#   # if( is.null(fname) ) fname = c('file.cio', cio$file)
#   #
#   # # special argument '.' copies all files
#   # if( all(fname=='.') ) fname = list.files(from, include.dirs=FALSE)
#   #
#   # # define source and destination file paths and check for existing ones
#   # dest.path = file.path(to, fname)
#   # dest.exists = file.exists(dest.path)
#   # src.path = file.path(from, fname)
#   # src.exists = file.exists(src.path)
#   #
#   # # check for and fix missing files listed in `fname`
#   # if( !all(src.exists) )
#   # {
#   #   # update the source and destination file lists to remove missing items
#   #   fname = fname[src.exists]
#   #   src.path = src.path[src.exists]
#   #   src.exists =  src.exists[src.exists]
#   #   dest.path = dest.path[src.exists]
#   #   dest.exists = dest.exists[src.exists]
#   # }
#   #
#   # # make list of existing destination files (possibly empty)
#   # fname.overwrite = fname[dest.exists]
#   #
#   # # handle existing files when overwrite not enabled
#   # if( ( !overwrite ) & ( length(fname.overwrite) > 0 ) )
#   # {
#   #   # warn of overwrites requested
#   #   msg1 = 'the following files already exist in directory'
#   #   msg2 = '(change `to` or set `overwrite=TRUE` to overwrite):\n'
#   #   warning(paste(msg1, to, msg2, paste(fname.overwrite, collapse=', ')))
#   #
#   #   # update the source and destination file lists to remove conflicts
#   #   idx.overwrite = fname %in% fname.overwrite
#   #   fname = fname[!idx.overwrite]
#   #   src.path = src.path[!idx.overwrite]
#   #   src.exists =  src.exists[!idx.overwrite]
#   #   dest.path = dest.path[!idx.overwrite]
#   #   dest.exists = dest.exists[!idx.overwrite]
#   #   fname.overwrite = character(0)
#   # }
#   #
#   # # count number of files to be written
#   # n.tocopy = length(fname)
#   #
#   # # error if we are left with nothing to write, then create directory as needed
#   # if( n.tocopy == 0 ) stop('Nothing to write')
#   # my_dir(to)
#   #
#   # # copy the files in a loop
#   # if( !quiet ) cat( paste('copying', n.tocopy, 'file(s) to', basename(to), '...\n') )
#   # if( !quiet ) pb = txtProgressBar(max=n.tocopy, style=3)
#   # for(idx.file in 1:n.tocopy)
#   # {
#   #   file.copy(src.path[idx.file], dest.path[idx.file], overwrite=overwrite)
#   #   if( !quiet ) setTxtProgressBar(pb, idx.file)
#   # }
#   # if( !quiet ) close(pb)
#   #
#   # # reload any config files that were replaced in current SWAT+ project directory
#   # if( overwrite & ( length(fname.overwrite) > 0 ) & (to == textio) )
#   # {
#   #   # handle 'file.cio' replacements by refreshing files list
#   #   if('file.cio' %in% fname.overwrite)
#   #   {
#   #     # maintain ignored files list
#   #     ignore = cio %>% filter(ignored) %>% pull(file)
#   #     cio = rswat_cio(ciopath, trim=FALSE, ignore=ignore, quiet=TRUE)
#   #   }
#   #
#   #   # check for overwrites that coincide with currently loaded files and reload them (if any)
#   #   fname.loaded = cio %>% filter( !is.na(ntab) ) %>% filter( !ignored )
#   #   idx.coinc = fname.overwrite %in% fname.loaded
#   #   if( any(idx.coinc) )
#   #   {
#   #     if( !quiet ) cat( paste('reloading', sum(idx.coinc), 'file(s)...') )
#   #     rswat_open(fname.overwrite[idx.coinc], reload=TRUE, quiet=TRUE)
#   #   }
#   # }
#   #
#   # # end the console messages and return the paths to the files written
#   # if( !quiet ) cat('done\n')
#   # return(dest.path)
# }


