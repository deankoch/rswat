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

