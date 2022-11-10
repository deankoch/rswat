
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
#' `df_stor[[f]]`. Every table loaded in this way is also copied to a large data frame
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
#' @field df_stor nested list of data frames, (internal use) file contents as tables
#'
rswat_db = setRefClass('rswat_db',

  fields = list(swat_dir = 'character',
                exe_path = 'character',
                cio_df = 'data.frame',
                line_df = 'data.frame',
                line_df_temp = 'data.frame',
                txt = 'list',
                df_stor = 'list'),

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
      df_stor <<- list()

      callSuper(...)
    },

    # project directory getter, setter, and validator
    get_swat_dir = function() swat_dir,
    set_swat_dir = function(d) swat_dir <<- rswat_validate_dpath(d, 'swat_dir'),
    check_swat_dir = function(d=swat_dir) invisible(rswat_validate_dpath(d, 'swat_dir')),

    # executable path getter, setter, and validator
    get_exe_path = function() exe_path,
    set_exe_path = function(p) exe_path <<- rswat_validate_fpath(p,'.exe', 'exe_path'),
    check_exe_path = function(p=exe_path) invisible(rswat_validate_fpath(p,'.exe', 'exe_path'))

  )
)

# define some more simple methods for rswat objects
rswat_db$methods( list(

  # console printout
  show = function() {

    # check for loaded files
    dates_loaded = FALSE
    config_files_msg = NULL
    files_loaded = cio_df[['file']][ cio_df[['loaded']] == TRUE ]
    file_cio_loaded = ifelse(length(files_loaded) == 0, FALSE, 'file.cio' %in% files_loaded)
    if(file_cio_loaded)
    {
      # build a message about files loaded
      n_known = sum(cio_df[['known']])
      n_loaded = sum(cio_df[['loaded']])
      n_group = length(unique(cio_df[['group']]))
      config_files_msg = paste0(n_known, ' config files in ',
                                n_group, ' groups (',
                                n_loaded, ' loaded)')

      # check for simulation dates among loaded files
      dates_loaded = all(c(sim='time.sim', prt='print.prt') %in% files_loaded)

      # build a message about simulation dates
      dates_msg = NULL
      if(dates_loaded)
      {
        # fetch the dates
        dates = rswat_time()
        n_sim = paste0('(', difftime(dates[['sim']][2], dates[['sim']][1], units='days'), ' days)')
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
    if(!is.na(exe_path)) exe_path_msg = paste('✓  simulator: ', exe_path, '\n')
    if(file_cio_loaded) file_cio_msg = paste('✓   file.cio:', config_files_msg, '\n')
    if(dates_loaded)
    {
      time_sim_msg = paste('✓   time.sim:', dates_sim_msg, '\n')
      print_prt_msg = paste('✓  print.prt:', dates_prt_msg, '\n')
    }

    hbreak_msg = '------------- - - - - - -'
    message('rswat')
    message(hbreak_msg)
    cat(paste0(swat_dir_msg, exe_path_msg))
    message(hbreak_msg)
    cat(paste0(time_sim_msg, print_prt_msg))
    message(hbreak_msg)
    cat(file_cio_msg)

    # cat(paste0('rswat\n',
    #            '-------------\n',
    #            swat_dir_msg,
    #            exe_path_msg,
    #            '-------------\n',
    #            time_sim_msg,
    #            print_prt_msg,
    #            '-------------\n',
    #            file_cio_msg))
  },

  # file info getter with options for subsets
  get_cio_df = function(what=NULL, f=NULL) {

    # by default returns all info
    if( is.na(get_swat_dir()) ) stop('project directory must be assigned first')
    if( is.null(f) ) f = cio_df[['file']]
    if( is.null(what) ) what = names(cio_df)
    return( cio_df[cio_df[['file']] %in% f, names(cio_df) %in% what] )
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
    known_files = cio_df[['file']]
    is_loaded = cio_df[['loaded']]
    if( any(is_loaded) )
    {
      # identify relevant rows of linedf then compute stats by file
      is_relevant = line_df[['file']] %in% known_files[is_loaded]
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
      idx_update = match(linedf_stats[['file']], known_files)
      cio_df[idx_update, nm_stats] <<- linedf_stats[nm_stats]
      rownames(cio_df) <<- seq(nrow(cio_df))
    }
  }
))





# # methods for compiling info on project directory
# rswat_db$methods( list(
#
# ))
