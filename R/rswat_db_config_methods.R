#' Methods for opening and parsing SWAT+ config files
#'
#' These define further methods for an rswat_db reference class object, related
#' to opening configuration files
#'
#' @include rswat_db.R
#' @name rswat_db_config_methods
rswat_db$methods( list(

  # opens a batch of config files in a loop
  open_config_batch = function(f=NULL, ignore=.rswat_gv_exclude(), quiet=FALSE) {

    # read file.cio and refresh files list
    parse_file_cio(refresh=TRUE, quiet=quiet)

    # # make a default list of files to open
    # if( is.null(f) ) f = cio_df |> dplyr::filter(!is.na(group)) |> pull(file)
    # msg_unknown = paste(f, collapse=', ')

    if( !any(get_cio_df(what='exists', f=f)) ) stop('file(s) not found')

    # # exclude ineligible files
    # f = cio_df |> dplyr::filter(exists) |>
    #   dplyr::filter( !is.na(type) ) |>
    #   dplyr::filter( type != 'log' ) |>
    #   dplyr::filter( file %in% f ) |>
    #   dplyr::filter( !(file %in% ignore) ) |>
    #   dplyr::filter( !(type %in% ignore) ) |>
    #   dplyr::filter( !(group %in% ignore) ) |>
    #   dplyr::pull(file)

    # skip if there were no eligible files
    n_files = length(f)
    if(n_files > 0)
    {
      # remove any existing data that would be replaced below
      line_df <<- line_df[!(line_df[['file']] %in% f),]
      stor_df <<- stor_df[!(names(stor_df) %in% f)]

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

    # skipped for empty files
    if( length(stor_df[[f]]) > 0 )
    {
      # set the 'loaded' flag in files metadata and directory list
      is_new = which(cio_df[['file']] == f)
      cio_df[['loaded']][is_new] <<- TRUE
      line_df_temp[['loaded']] <<- TRUE

      # add type and group columns to temporary variables data frame
      line_df_temp[['type']] <<- cio_df[['type']][is_new]
      line_df_temp[['group']] <<- cio_df[['group']][is_new]

      # copy contents of temporary data frame to the persistent one then tidy row names
      line_df <<- rbind(subset(line_df, file != f), line_df_temp)
      rownames(line_df) <<- seq(nrow(line_df))

      # parse file.cio to update groups
      if(f == 'file.cio') parse_file_cio(refresh=FALSE, quiet=TRUE)
    }

    # update OS file info
    if(update_stats) stats_cio_df()
    if(output) return(stor_df[[f]])
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
    if( output & !refresh & ( f %in% names(stor_df) ) ) return(stor_df[[f]])

    # `f` must be listed in `cio_df`. Grab the file path from there
    read_path = cio_df |> dplyr::filter(file==f) |> dplyr::select(path) |> as.character()
    msg_refresh = 'file not recognized. Try refreshing the project directory with refresh_cio_df'
    if( length(read_path) == 0 ) stop(msg_refresh)

    # count all available table number(s) then load tables in a loop
    table_num = line_df_temp[['table']] |> unique(na.rm=TRUE) |> sort()
    stor_df[[f]] <<- lapply(table_num, \(tn) rswat_rtable_txt(line_df_temp, read_path, tn) )

    # fix for weird table structure in weather generators file
    if(f == 'weather-wgn.cli') table_num = fix_weather_tables()

    # copy variable names from fread calls, in a loop over table numbers
    for(tn in table_num)
    {
      # find relevant rows of temporary metadata list
      is_table = line_df_temp[['table']] == tn

      # find mapping from parameters data frame to line_df based on field order and header spacing
      nm_header = names(stor_df[[f]][[tn]])
      n_col = line_df_temp[['field_num']][is_table] |> max() |> pmin(length(nm_header))
      idx_col_j = lapply(seq(n_col), \(j) which(line_df_temp[['field_num']][is_table] == j))

      # assign variable 'name' (writes to both tabular and header entries)
      header_j = lapply(seq(n_col), \(j) rep(nm_header[j], length(idx_col_j[[j]])) )
      line_df_temp[['name']][is_table][ unlist(idx_col_j) ] <<- unlist(header_j)

      # repeat for 'class' (writes to both tabular and header entries)
      nm_class = unlist( lapply(stor_df[[f]][[tn]], class) )
      class_j = lapply(seq(n_col), \(j) rep(nm_class[j], length(idx_col_j[[j]])) )
      line_df_temp[['class']][is_table][ unlist(idx_col_j) ] <<- unlist(class_j)

      # find the known precision levels (based on first row of table)
      nm_nprec = lapply(idx_col_j, \(j) {

        # the ifelse handles cases where an entry is missing or the table has no rows
        line_df_temp[['n_prec']][is_table][ifelse(length(j) > 1, j[2], NA) ]
      })

      # copy precision info to headers
      is_header = is_table & line_df_temp[['header']]
      n_copy = min(sum(is_header), length(unlist(nm_nprec)))
      line_df_temp[['n_prec']][is_header][seq(n_copy)] <<- unlist(nm_nprec)[seq(n_copy)]
    }

    if(output) return(stor_df[[f]])
  },

  # get data frame summarizing the text field data, copying comment to cio_df
  parse_config_txt = function(f=NULL, output=TRUE) {

    # return an empty data frame with named columns when no f supplied
    if(is.null(f)) return(rswat_scan_txt())

    # load plain text fields and build a data frame of info about them
    line_df_temp <<- get_config_txt(f) |>
      rswat_scan_txt(f) |>
      rswat_ftable_txt()

    # if the data frame is empty (empty file), then we are done
    if(nrow(line_df_temp) == 0) return(line_df_temp)

    # TODO: some hacking may be required for exceptional files like gwflow, weather.wgn here?

    # assign precisions
    line_df_temp <<- rswat_n_prec_txt(line_df_temp, quiet=TRUE)

    # copy comment (if any) before returning
    if(f %in% names(txt)) cio_df[['msg']][ which(cio_df[['file']] == f) ] <<- txt[[f]][[1]]
    if(output) return(line_df_temp)
    return(invisible())
  },

  # load plaintext as a list of character strings (one per line)
  get_config_txt = function(f, n_line=Inf, output=TRUE) {

    # `f` must be listed in `cio_df`. Grab the file path from there
    msg_refresh = paste('file', f, 'not found. Try calling rswat() again to scan for new files')
    idx_cio = which( cio_df[['file']] == f )
    if( length(idx_cio) == 0 ) stop(msg_refresh)
    read_path = cio_df[['path']][idx_cio]
    if( is.na(read_path) ) stop(msg_refresh)

    # calculate the file hash and compare with previous (if any)
    new_hash = rlang::hash_file(read_path)
    old_hash = cio_df[['hash_load']][idx_cio]
    if( length(old_hash) == 0 ) old_hash = NA
    is_new = ifelse(is.na(old_hash), TRUE, new_hash==old_hash)

    # use cached data if available
    if( !(f %in% names(txt)) | is_new )
    {
      # read line-by-line text from disk and update the hash
      n_readLines = ifelse(is.infinite(n_line), -1, n_line)
      txt[[f]] <<- readLines(read_path, n=n_readLines)
      cio_df[['hash_load']][idx_cio] <<- new_hash
    }

    if(output) return(txt[[f]])
    return(invisible())
  },

  # parse file.cio and add group labels to cio_df
  parse_file_cio = function(refresh=TRUE, quiet=FALSE) {

    # check that the file.cio text is in cache (not the same as is_file_loaded!)
    is_loaded = 'file.cio' %in% names(stor_df)
    if(!is_loaded) stop('file.cio not loaded. Try calling open_config_file first')
    file_cio_df = stor_df[['file.cio']][[1]]

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
      cio_df <<- data.frame(file=unknown_files, exists=FALSE, loaded=FALSE) |>
        dplyr::full_join(cio_df, by=c('file', 'exists', 'loaded'))
    }

    # write group names to known files in directory listing
    is_listed = cio_df[['file']] %in% cio_group[['file']][!is_unknown]
    idx_group = cio_df[['file']][is_listed] |> match(cio_group[['file']])
    cio_df[['group']][is_listed] <<- cio_group[['group']][idx_group]

    # update flags
    is_cio = cio_df[['file']] == 'file.cio'
    cio_df[['group']][ is_cio ] <<- 'cio'
    cio_df[['loaded']][ is_cio ] <<- TRUE
    cio_df[['type']][ is_listed | is_cio ] <<- 'config'
    #cio_df[['known']] <<- !is.na(cio_df[['type']])

    # TODO: deal with weather data files
    # assign weather groups
    # weather_fname = rswat_gv('weather_fname')
    # weather_match = lapply(weather_fname, \(s) which(cio[['file']] == s))
    # for( j in seq(nrow(weather_fname)) ) cio[['group']][ weather_match[[j]] ] = names(weather_fname)[j]
    # assign groups to weather data files
    #print(rswat_gv('weather_fname'))

    # set factor levels for groups with cio first
    group_order = c('cio', unique(cio_group[['group']]))

    # reorder the rows so they match the group order in file.cio
    idx_reorder = cio_df[['group']] |> factor(levels=group_order) |> order()
    cio_df <<- cio_df[idx_reorder,]
    rownames(cio_df) <<- seq(nrow(cio_df))
  },

  # hack to present weather data as a set of station tables plus a single locations table
  fix_weather_tables = function() {

    table_num_vec = line_df_temp[['table']]
    all_num = table_num_vec |> unique(na.rm=TRUE) |> sort()
    if( length(all_num) == 0 ) stop('table data not found. Have you loaded weather-wgn.cli?')

    # split file by table index, producing a vector of line numbers for each one
    table_dat = stor_df[['weather-wgn.cli']]
    table_idx_list = split(seq(table_num_vec), table_num_vec)

    # put the rows in same order as in file (table ids are negative)
    idx_merge = max(all_num) |> seq()
    idx_nomerge = length(idx_merge) + idx_merge
    table_merged = do.call(rbind, table_dat[rev(idx_merge)])

    # new table index for merged data
    table_num_new = 1L + length(idx_merge)
    attr(table_merged, 'rswat_table_num') = table_num_new

    # copy restructured list back to file contents storage
    stor_df[['weather-wgn.cli']] <<- c(table_dat[idx_nomerge], list(table_merged))

    # replace old table numbers with merged ones and update flags
    table_idx_merged = do.call(c, table_idx_list[idx_merge]) |> unname()
    line_df_temp[['table']][table_idx_merged] <<- table_num_new
    line_df_temp[['name']][table_idx_merged] <<- names(table_merged)

    # return the new set of table numbers
    line_df_temp[['table']] |> unique(na.rm=TRUE) |> sort()
  }

))
