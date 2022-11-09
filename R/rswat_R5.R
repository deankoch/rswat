
#' An rswat_path object stores the project directory and executable path
#'
#' swat_dir is the path to the SWAT+ project directory (should contain "file.cio")
#' exe_path is the path to the SWAT simulation executable (path should end in ".exe")
#'
#' @import methods
#' @export
#' @exportClass rswat
rswat_generator = setRefClass('rswat',

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
    get_swar_dir = function() swat_dir,
    set_swar_dir = function(d) swat_dir <<- rswat_validate_dpath(d, 'swat_dir'),
    check_swat_dir = function(d=swat_dir) invisible(rswat_validate_dpath(d, 'swat_dir')),

    # executable path getter, setter, and validator
    get_exe_path = function() exe_path,
    set_exe_path = function(p) exe_path <<- rswat_validate_fpath(p,'.exe', 'exe_path'),
    check_exe_path = function(p=exe_path) invisible(rswat_validate_fpath(p,'.exe', 'exe_path'))

  )
)

# define some more simple methods for rswat objects
rswat_generator$methods( list(

  # console printout
  show = function() {

    swat_dir_msg = '• project directory: not assigned'
    exe_path_msg = '• SWAT+ executable: not assigned'
    if(!is.na(swat_dir)) swat_dir_msg = paste('✓ project directory:', swat_dir)
    if(!is.na(exe_path)) exe_path_msg = paste('✓ SWAT+ executable:', exe_path)
    cat(paste('rswat\n', swat_dir_msg, '\n', exe_path_msg))
  },

  # file info getter with options for subsets
  get_cio_df = function(what=NULL, fname=NULL) {

    # by default returns all info
    if( is.na(get_swar_dir()) ) stop('project directory must be assigned first')
    if( is.null(fname) ) fname = cio_df[['file']]
    if( is.null(what) ) what = names(cio_df)
    return( cio_df[cio_df[['file']] %in% fname, names(cio_df) %in% what] )
  },

  # refresh cio_df by scanning project directory and labeling files based on their extension
  refresh_cio_df = function() {

    if(is.na(swat_dir)) stop('project directory must be assigned first')
    cio_df <<- rswat_scan_dir(swat_dir, cio_df)
  }
))


# methods for opening and parsing SWAT+ config files
rswat_generator$methods( list(

  # opens a batch of config files in a loop
  open_config_batch = function(f=NULL, ignore=.rswat_gv_ignore(), quiet=FALSE) {

    # read file.cio and refresh files list
    parse_file_cio(refresh=TRUE, quiet=quiet)

    # make a default list of files to open
    if( is.null(f) ) f = cio_df |> dplyr::filter(type=='config') |> pull(file)
    msg_unknown = paste(f, collapse=', ')

    # exclude ineligible files
    f = cio_df |> dplyr::filter(exists) |>
      dplyr::filter( !is.na(type) ) |>
      dplyr::filter( type != 'log' ) |>
      dplyr::filter( file %in% f ) |>
      dplyr::filter( !(file %in% ignore) ) |>
      dplyr::filter( !(type %in% ignore) ) |>
      dplyr::filter( !(group %in% ignore) ) |>
      dplyr::pull(file)

    # skip if there were no eligible files
    n_files = length(f)
    if(n_files > 0)
    {
      # removed stored data that will be replaced below
      line_df <<- line_df[!(line_df[['file']] %in% f),]
      df_stor <<- df_stor[!(names(df_stor) %in% f)]

      # build console progress messages with fixed width, and initialize a progress bar object
      if(!quiet)
      {
        cat(paste('\nloading', n_files, 'file(s)'))
        msg_width = nchar(f) |> max()
        msg_pad = sapply(f, \(g) paste0(rep(' ', msg_width - nchar(g)), collapse=''))
        msg_progress = paste(' > ', paste(f, msg_pad))
        pb_width = getOption('width') - max(nchar(msg_progress)) - 5
        pb = utils::txtProgressBar(max=1L+n_files, style=3L, width=pb_width)
      }

      # loop over files, loading data into package storage
      line_df_list = vector(mode='list', length=n_files) |> stats::setNames(f)
      for(fname in f)
      {
        # update progress bar prior to loading the file
        if(!quiet)
        {
          idx_progress = which(f==fname)
          setTxtProgressBar(pb, idx_progress)
          msg_progress[idx_progress] |> cat()
          flush.console()
        }

        # load the file and copy to storage, waiting until the end to update stats
        open_config_file(fname, output=FALSE, update_stats=FALSE)
      }

      # update stats
      stats_cio_df()

      # tidy final state for progress bar message
      if(!quiet)
      {
        setTxtProgressBar(pb, 1L+length(f))
        close(pb)
        cat('\n')
      }

    }
  },

  # open a swat config file
  open_config_file = function(f, output=TRUE, refresh=TRUE, update_stats=TRUE) {

    # open and parse the file (populate line_df_temp) then copy tables to data frames in storage
    get_config_tables(f, output=FALSE, refresh=refresh)

    # set the 'loaded' flag in files metadata and directory list
    line_df_temp[['loaded']] <<- TRUE
    cio_df[['loaded']][ which(cio_df[['file']] == f) ] <<- TRUE

    # copy contents of temporary data frame to the persistent one then tidy row names
    line_df <<- rbind(subset(line_df, file != f), line_df_temp)
    rownames(line_df) <<- seq(nrow(line_df))

    # update files stats before returning
    if(update_stats) stats_cio_df()
    if(output) return(df_stor[[f]])
  },

  # get a list of data frames representing the parameters in a SWAT+ config file
  get_config_tables = function(f, output=TRUE, refresh=TRUE) {

    # use cached data if available and when refresh not requested
    if( refresh | !( f %in% line_df_temp[['file']] ) )
    {
      # read and parse text from disk. This silently writes line_df_temp
      parse_config_txt(f, output=FALSE)
      refresh = TRUE
    }

    # return cached output if available and when refresh not requested
    if( output & !refresh & ( f %in% names(df_stor) ) ) return(df_stor[[f]])

    # `f` must be listed in `cio_df`. Grab the file path from there
    read_path = cio_df |> dplyr::filter(file==f) |> dplyr::select(path) |> as.character()
    if( length(read_path) == 0 ) stop('file not recognized. Try refreshing the project directory')

    # count all available table number(s) then load tables in a loop
    table_num = line_df_temp[['table']] |> unique(na.rm=TRUE) |> sort()
    df_stor[[f]] <<- lapply(table_num, \(tn) rswat_rtable_txt(line_df_temp, read_path, tn) )

    # fix for weird table structure in weather generators file
    if(f == 'weather-wgn.cli') table_num = fix_weather_tables()

    # copy variable names from fread calls, in a loop over table numbers
    for(tn in table_num)
    {
      # find relevant rows of temporary metadata list
      is_table = line_df_temp[['table']] == tn

      # assign variable 'name' based on field order and header spacing
      nm_header = names(df_stor[[f]][[tn]])
      n_col = line_df_temp[['field_num']][is_table] |> max() |> pmin(length(nm_header))
      idx_colj = lapply(seq(n_col), \(j) which(line_df_temp[['field_num']][is_table] == j))
      header_j = lapply(seq(n_col), \(j) rep(nm_header[j], length(idx_colj[[j]])) )

      # copy to temporary metadata list
      line_df_temp[['name']][is_table][ unlist(idx_colj) ] <<- unlist(header_j)
    }

    if(output) return(df_stor[[f]])
  },

  # get data frame summarizing the text field data, copying comment to cio_df
  parse_config_txt = function(f=NULL, output=TRUE) {

    # return an empty data frame with named columns when no f supplied
    if(is.null(f)) return(rswat_scan_txt())

    # load plain text fields and build a data frame of info about them
    line_df_temp <<- get_config_txt(f) |>
      rswat_scan_txt(f) |>
      rswat_ftable_txt()

    # TODO: some hacking may be required for exceptional files like gwflow, weather.wgn here?

    # assign precisions
    line_df_temp <<- rswat_nprec_txt(line_df_temp)

    # copy comment (if any) before returning
    if(f %in% names(txt)) cio_df[['msg']][ which(cio_df[['file']] == f) ] <<-  txt[[f]][[1]]
    if(output) return(line_df_temp)
    return(invisible())
  },

  # load plaintext as a list of character strings (one per line)
  get_config_txt = function(f, n_line=Inf, output=TRUE) {

    # `f` must be listed in `cio_df`. Grab the file path from there
    read_path = cio_df |> dplyr::filter(file==f) |> dplyr::select(path) |> unlist()
    if( length(read_path) == 0 ) stop('file not recognized. Try refreshing the project directory')

    # use cached data if available
    if( !(f %in% names(txt)) )
    {
      # read line-by-line text from disk when requested
      n_readLines = ifelse(is.infinite(n_line), -1, n_line)
      txt[[f]] <<- readLines(read_path, n=n_readLines)
    }

    if(output) return(txt[[f]])
    return(invisible())
  },

  # hack to present weather data as a set of station tables plus a single locations table
  fix_weather_tables = function() {

    table_num_vec = line_df_temp[['table']]
    all_num = table_num_vec |> unique(na.rm=TRUE) |> sort()
    if( length(all_num) == 0 ) stop('table data not found. Have you loaded weather-wgn.cli?')

    # split file by table index, producing a vector of line numbers for each one
    table_dat = df_stor[['weather-wgn.cli']]
    table_idx_list = split(seq(table_num_vec), table_num_vec)

    # put the rows in same order as in file (table ids are negative)
    idx_merge = max(all_num) |> seq()
    idx_nomerge = length(idx_merge) + idx_merge
    table_merged = do.call(rbind, table_dat[rev(idx_merge)])

    # new table index for merged data
    table_num_new = 1L + length(idx_merge)
    attr(table_merged, 'rswat_table_num') = table_num_new

    # copy restructured list back to file contents storage
    df_stor[['weather-wgn.cli']] <<- c(table_dat[idx_nomerge], list(table_merged))

    # replace old table numbers with merged ones and update flags
    table_idx_merged = do.call(c, table_idx_list[idx_merge]) |> unname()
    line_df_temp[['table']][table_idx_merged] <<- table_num_new
    line_df_temp[['name']][table_idx_merged] <<- names(table_merged)

    # return the new set of table numbers
    line_df_temp[['table']] |> unique(na.rm=TRUE) |> sort()
  }

))


# methods for compiling info on project directory
rswat_generator$methods( list(

  # parse file.cio and add group labels to cio_df
  parse_file_cio = function(refresh=TRUE, quiet=FALSE) {

    # refresh files list and load the file
    refresh_cio_df()
    f = 'file.cio'
    file_cio_df = open_config_file(f, refresh=refresh)[[1]]

    # load up to date list of existing files
    existing_files = cio_df[['file']][ cio_df[['exists']] ]

    # read/parse text file.cio text and build table of file and group names
    null_strings = c('', 'null')
    cio_by_group = apply(file_cio_df, 1, \(x) data.frame(group=rep(x[1], length(x)-1), file=x[-1]))
    cio_group = do.call(rbind, cio_by_group) |> dplyr::filter( !(file %in% null_strings) )

    # deal with files listed in file.cio, but not found in swat_dir
    is_unknown = !( cio_group[['file']] %in% existing_files )
    if( any(is_unknown) )
    {
      # report the files on console
      unknown_files = cio_group[['file']][is_unknown]
      files_msg = paste(unknown_files, collapse=', ')
      missing_msg = 'file(s) listed in file.cio but not found on disk:'
      if(!quiet) paste(sum(is_unknown), missing_msg, files_msg, '\n') |> cat()

      # append the missing files to directory listing but flag as non-existent
      cio_df <<- data.frame(file=unknown_files, known=FALSE, exists=FALSE, loaded=FALSE) |>
        dplyr::full_join(cio_df, by=c('file', 'known', 'exists', 'loaded'))
    }

    # write group names to known files in directory listing
    is_listed = cio_df[['file']] %in% cio_group[['file']][!is_unknown]
    idx_group = cio_df[['file']][is_listed] |> match(cio_group[['file']])
    cio_df[['group']][is_listed] <<- cio_group[['group']][idx_group]

    # update flags
    is_cio = cio_df[['file']] == 'file.cio'
    cio_df[['group']][ is_cio ] <<- 'cio'
    cio_df[['loaded']][ is_cio ] <<- TRUE
    cio_df[['known']][ is_listed | is_cio ] <<- TRUE
    cio_df[['type']][ is_listed | is_cio ] <<- 'config'

    # TODO: deal with weather data files
    # assign weather groups
    # weather_fname = rswat_gv('weather_fname')
    # weather_match = lapply(weather_fname, \(s) which(cio[['file']] == s))
    # for( j in seq(nrow(weather_fname)) ) cio[['group']][ weather_match[[j]] ] = names(weather_fname)[j]
    # assign groups to weather data files
    #print(rswat_gv('weather_fname'))

    # reorder the rows so they match the group order in file.cio
    idx_reorder = cio_df[['group']] |> factor(unique(cio_group[['group']])) |> order()
    cio_df <<- cio_df[idx_reorder,]

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
    }
  }

))
