
#' A reference class for SWAT+ projects
#'
#' An "rswat_db" object is a reference class (R5) object that collects information
#' and methods for a SWAT+ project, based on the configuration files found in the SWAT+
#' project directory `swat_dir`. Get a new instance of an "rswat_db" object by calling the
#' `new()` method with `rs = rswat_db$new()`
#'
#' Project files (discovered in `swat_dir`) are listed in the `cio_df` field, a data frame
#' with one row per file and various attributes to describe and categorize the files. Only
#' files listed in this data frame can be loaded.
#'
#' When a file `f` is loaded, it is parsed as a list of data frames which are copied to
#' `stor_df[[f]]`. Every table loaded in this way is also copied to a large data frame
#' `line_df`, which has a row for each white-space delimited field (including headers),
#' from all loaded files. A third copy of the data is stored in `txt[[f]]`, which caches
#' the line-by-line text of the file as a vector of character strings.
#'
#' @section methods:
#' The typical use case is to assign the project directory (`swat_dir`), scan for files
#' and open "file.cio" (to make `cio_df`), then open a some/all of the rest of the
#' configuration files (to make `line_df`). For example,
#'
#' * set field `swat_dir` either on initialization or via method `set_swar_dir`
#' * get file info by calling `open_config_file('file.cio')` (populates field `cio_df`)
#' * load files listed in `f_list` by calling `open_config_batch(f_list)` (populates field `line_df`)
#' * open one of these files `f` by calling `get_df(f)` (returns list of data frames)
#'
#' @section .rswat_db:
#' When the `rswat` package is loaded, an rswat_db class object called `.rswat_db` is defined
#' to store all data related to the currently loaded project. Non-developers should avoid
#' interacting directly with `.rswat_db` (ie don't call its methods or set its fields) and
#' instead use the provided helper functions.
#'
#' @section Reference class R5:
#' R5 objects behave like traditional OOP classes you would find in C++ or Python, and
#' this is quite different from the value semantics used with other R objects.
#'
#' For example, R5 methods come bundled with the object itself (encapsulation), and they
#' are accessed using the `$` operator, the same way that we access list entries.
#'
#' R5 objects also have reference semantics, meaning that if you "copy" `rs` by assignment
#' with `x = rs`, then modify `x`, you are actually modifying `rs`. This is helpful for
#' speeding up loading and parsing times (less data copied as function arguments), and it
#' makes it easier to maintain a database about SWAT+ parameters that is visible to
#' all `rswat` functions.
#'
#' In the code for methods, a special assignment operator (see `?<<-`) is used
#' extensively to write to fields of the parent rswat_db object. When we need to reference
#' this object explicitly (like in arguments to external function calls), we use `.self`.
#'
#' In other functions (those which aren't rswat_db methods) we reference the rswat_db object
#' via the parameter `.db`, which by default points to `.rswat_db`.
#'
#' @name rswat_db
#' @import methods
#' @exportClass rswat_db
#'
#' @field swat_dir character, path to the SWAT+ project directory (should contain "file.cio")
#' @field exe_path character, path to the SWAT simulation executable (path should end in ".exe")
#' @field cio_df data frame, (internal use) information about files in the directory
#' @field line_df data frame, (internal use) information about all file contents
#' @field line_df_temp data frame, (internal use) information about specific file contents
#' @field txt list of character vectors, (internal use) file contents as plain text
#' @field stor_df nested list of data frames, (internal use) file contents as tables
#'
rswat_db = setRefClass('rswat_db',

  fields = list(swat_dir = 'character',
                exe_path = 'character',
                cio_df = 'data.frame',
                line_df = 'data.frame',
                line_df_temp = 'data.frame',
                txt = 'list',
                stor_df = 'list',
                docs = 'list'),

  methods = list(

    # constructor to set defaults
    initialize = function(...) {

      # this must be supplied by the user
      swat_dir <<- NA_character_

      # optional (required for simulations)
      exe_path <<- NA_character_

      # initialize empty data frame to store directory info,
      cio_df <<- rswat_scan_dir()

      # initialize empty data frames to store file metadata
      line_df <<- rswat_scan_txt()
      line_df_temp <<- rswat_scan_txt()

      # initialize empty storage list for file contents
      txt <<- list()
      stor_df <<- list()

      # this list loaded from sysdata.rda
      docs <<- .rswat_io_pdf

      callSuper(...)
    },

    # project directory getter, setter, and validator
    get_swat_dir = function() swat_dir,
    set_swat_dir = function(d) swat_dir <<- rswat_validate_dpath(d, 'swat_dir'),
    check_swat_dir = function(d=swat_dir) invisible(rswat_validate_dpath(d, 'swat_dir')),

    # executable path getter, setter, and validator
    get_exe_path = function() exe_path,
    set_exe_path = function(p) exe_path <<- rswat_validate_fpath(p,'.exe', 'exe_path'),
    check_exe_path = function(p=exe_path) invisible(rswat_validate_fpath(p,'.exe', 'exe_path')),

    # file info getter with options for subsets
    get_cio_df = function(what=NULL, f=NULL, check_dir=TRUE) {

      # error when swat directory not assigned unless otherwise requested
      if( check_dir & is.na(swat_dir) ) stop('project directory must be assigned first')

      # by default returns all info
      if( is.null(f) ) f = cio_df[['file']]
      if( is.null(what) ) what = names(cio_df)
      return( cio_df[cio_df[['file']] %in% f, names(cio_df) %in% what] )
    },

    # SWAT+ data frames getter
    get_stor_df = function(f) {
      # halt if any of the requested files hasn't been loaded yet
      is_loaded = f %in% names(stor_df)
      missing_msg = paste('files ', paste(paste(f[!is_loaded], collapse=', ')), 'not found')
      if( !all(is_loaded) ) stop(missing_msg)

      # return from single file case
      if( length(f) == 1 ) return(stor_df[[f]])

      # return multiple files as named list
      return(stor_df[f])
    },

    # get list of loaded files
    get_loaded_files = function(check_dir=TRUE) {

      is_loaded = get_cio_df(what='loaded', check_dir=check_dir)
      return( get_cio_df(what='file', check_dir=check_dir)[ is_loaded ] )
    },

    # check if a file has been loaded yet
    is_file_loaded = function(f, check_dir=TRUE) {

      files_loaded = get_loaded_files(check_dir=check_dir)
      if( length(files_loaded) == 0 ) return(FALSE)
      return( f %in% files_loaded )
    },

    # returns a string summarizing file listed in file.cio
    report_known_files = function() {

      # returns empty string when file.cio hasn't been loaded
      if(!is_file_loaded('file.cio')) return('')

      # build a message about files loaded
      n_known = sum( get_cio_df(what='known') )
      n_group = length( unique( get_cio_df(what='group') ) )
      paste0(n_known, ' config files in ', n_group, ' groups')
    },

    # console printout
    show = function() {

      config_files_msg = NULL

      # check for file.cio and simulation dates among loaded files
      dates_loaded = is_file_loaded(c(sim='time.sim', prt='print.prt'), check_dir=FALSE)
      is_file_cio_loaded = is_file_loaded('file.cio', check_dir=FALSE)

      # print file info when file.cio has been loaded
      if(is_file_cio_loaded)
      {
        # build a message about files loaded
        n_known = sum( get_cio_df(what='known') )
        n_group = length( unique( get_cio_df(what='group') ) )
        config_files_msg = paste0(report_known_files(), '\n')

        # build a message about simulation dates
        dates_msg = NULL
        if(all(dates_loaded))
        {
          # fetch the dates (not an rswat_db method, so we pass a reference to calling frame)
          dates = rswat_time(.db=.self)
          step_msg = paste(' days,', dates[['step']], 'timestep)')
          n_sim = paste0('(', difftime(dates[['sim']][2], dates[['sim']][1], units='days'), step_msg)
          n_prt = paste0('(', difftime(dates[['prt']][2], dates[['prt']][1], units='days'), ' days)')

          # messages about simulation dates and printed output
          dates_sim_msg = paste(paste(paste0('[', dates[['sim']], ']'), collapse=' to '), n_sim)
          dates_prt_msg = paste(paste(paste0('[', dates[['prt']], ']'), collapse=' to '), n_prt)
        }
      }

      # different output depending on steps being completed
      swat_dir_msg = '•  directory: not assigned\n'
      exe_path_msg = '•  simulator: not assigned\n'
      time_sim_msg = '•   time.sim: not loaded\n'
      file_cio_msg = '•   file.cio: not loaded\n'
      print_prt_msg = '•  print.prt: not loaded\n'
      if(!is.na(swat_dir)) swat_dir_msg = paste('✓  directory:', swat_dir, '\n')
      if(!is.na(exe_path)) exe_path_msg = paste('✓  simulator:', exe_path, '\n')
      if(is_file_cio_loaded) file_cio_msg = paste('✓   file.cio:', config_files_msg, '\n')
      if(all(dates_loaded))
      {
        time_sim_msg = paste('✓   time.sim:', dates_sim_msg, '\n')
        print_prt_msg = paste('✓  print.prt:', dates_prt_msg, '\n')
      }

      hbreak_msg = '  -----------'
      message('rswat')
      message(hbreak_msg)
      cat(paste0(swat_dir_msg, exe_path_msg))
      message(hbreak_msg)
      cat(paste0(time_sim_msg, print_prt_msg))
      message(hbreak_msg)
      cat(file_cio_msg)
    },

    # refresh cio_df by scanning project directory and labeling files based on their extension
    refresh_cio_df = function() {

      if(is.na(swat_dir)) stop('project directory must be assigned first')
      new_cio_df = rswat_scan_dir(swat_dir, cio_df)
      cio_df <<- new_cio_df
      rownames(cio_df) <<- seq(nrow(cio_df))
    },

    # append metadata for any loaded files to cio_df
    stats_cio_df = function() {

      # append metadata for any loaded files
      all_files = cio_df[['file']]
      is_loaded = is_file_loaded(all_files)
      if( any(is_loaded) )
      {
        # identify relevant rows of linedf then compute stats by file
        is_relevant = line_df[['file']] %in% all_files[is_loaded]
        linedf_stats = line_df[is_relevant,] |> dplyr::group_by(file, table) |>
          dplyr::summarize(n_table = dplyr::n_distinct(table, na.rm=TRUE),
                           n_var = dplyr::n_distinct(name, na.rm=TRUE),
                           n_line = diff(range(line_num)),
                           n_skip = dplyr::n_distinct(line_num[skipped]),
                           .groups = 'drop_last') |>
          dplyr::summarize(n_table = sum(n_table),
                           n_var = sum(n_var),
                           n_line = sum(n_line),
                           n_skip = sum(n_skip)) |> as.data.frame()

        # update old fields
        nm_stats = c('n_line', 'n_skip', 'n_table', 'n_var')
        idx_update = match(linedf_stats[['file']], all_files)
        cio_df[idx_update, nm_stats] <<- linedf_stats[nm_stats]
        rownames(cio_df) <<- seq(nrow(cio_df))
      }
    }
  )
)
