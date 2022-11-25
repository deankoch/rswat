
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

    # variable info getter with options for subsets
    get_line_df = function(what=NULL, f=NULL, check_dir=TRUE, drop=FALSE) {

      # error when swat directory not assigned unless otherwise requested
      if( check_dir & is.na(swat_dir) ) stop('project directory must be assigned first')

      # by default returns all info
      if( is.null(f) ) f = line_df[['file']]
      if( is.null(what) ) what = names(line_df)
      return( line_df[line_df[['file']] %in% f, names(line_df) %in% what, drop=drop] )
    },

    # file info getter with options for subsets
    get_cio_df = function(what=NULL, f=NULL, check_dir=TRUE, drop=FALSE) {

      # error when swat directory not assigned unless otherwise requested
      if( check_dir & is.na(swat_dir) ) stop('project directory must be assigned first')

      # by default returns all info
      if( is.null(f) ) f = cio_df[['file']]
      if( is.null(what) ) what = names(cio_df)
      return( cio_df[cio_df[['file']] %in% f, names(cio_df) %in% what, drop=drop] )
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

      # drop=TRUE returns as vector instead of data frame
      is_loaded = get_cio_df(what='loaded', check_dir=check_dir, drop=TRUE)
      return( get_cio_df(what='file', check_dir=check_dir, drop=TRUE)[ is_loaded ] )
    },

    # check if a file has been loaded yet
    is_file_loaded = function(f=NULL, check_dir=TRUE) {

      files_loaded = get_loaded_files(check_dir=check_dir)
      if( length(f) == 0 ) return(FALSE)
      if( length(files_loaded) == 0 ) return( stats::setNames(rep(FALSE, length(f)), nm=f) )
      return( stats::setNames(f %in% files_loaded, nm=f) )
    },

    # returns a string summarizing file listed in file.cio
    report_known_files = function() {

      # returns empty string when file.cio hasn't been loaded
      if(!is_file_loaded('file.cio')) return('')

      # find config files and group
      is_config = get_cio_df(what='type', drop=TRUE) == 'config'
      is_config[ is.na(is_config) ] = FALSE
      n_config = sum(is_config)
      n_group = length( unique( get_cio_df(what='group', drop=TRUE)[is_config] ) )

      # build a message about files loaded
      paste0(n_config, ' config files in ', n_group, ' groups')
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
    },

    # get a data frame of start/end dates for the simulation as reported in time.sim or print.prt
    get_sim_dates = function(lazy=TRUE, prt=FALSE, render=TRUE) {

      # load check then open the file contents (first table)
      f = ifelse(prt, 'print.prt', 'time.sim')
      if( !lazy & !is_file_loaded(f) ) return( data.frame(date=as.Date(integer(0L))) )
      time_file = open_config_file(f)[[1L]]

      # extract integer representation of dates
      dates_as_int = rbind(start = c(jday=time_file[['day_start']], year=time_file[['yrc_start']]),
                           end = c(jday=time_file[['day_end']], year=time_file[['yrc_end']]))

      # Dates in data frame with two rows
      dates = rswat_date_conversion(dates_as_int)
      if(!render) return(dates)

      # or instead return a string for printing this information
      if( anyNA(dates) ) return(NA_character_)
      return( paste0('[', paste(dates[['date']], collapse=' to '), ']') )
    },

    # console printout
    show = function() rswat_summarize_db(.db=.self)
  )
)
