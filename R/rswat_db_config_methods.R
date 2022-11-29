#' Methods for opening and parsing SWAT+ config, weather, and output files
#'
#' @include rswat_db.R
#' @name rswat_db_config_methods
rswat_db$methods( list(

  # opens a batch of config files in a loop
  open_config_batch = function(f=NULL, refresh=FALSE, quiet=TRUE, update_stats=FALSE) {

    # refresh files list and remove any existing data that would be replaced below
    if( refresh )
    {
      refresh_cio_df()
      line_df <<- line_df[!(line_df[['file']] %in% f),]
      stor_df <<- stor_df[!(names(stor_df) %in% f)]
      cio_df[['loaded']][ cio_df[['file']] %in% f ] <<- FALSE
    }

    # check file existence
    n_files = sum( get_cio_df(what='exists', f=f, drop=TRUE) )
    if( n_files == 0 ) stop('file(s) not found')

    # build console progress messages with fixed width
    if(!quiet)
    {
      message(paste(n_files, 'file(s) requested\n'))
      msg_progress = paste('loading', f) |> rwat_progress()
    }

    # loop over files, loading data into package storage
    line_df_list = vector(mode='list', length=n_files) |> stats::setNames(f)
    for(fname in f)
    {
      # update progress bar prior to loading the file
      if(!quiet) msg_progress[which(f==fname)] |> cat()

      # load the file and copy to storage, waiting until the end to update stats
      open_config_file(fname, output=FALSE, refresh=FALSE, update_stats=FALSE)
    }

    # update stats
    if(update_stats) stats_cio_df()

    # tidy final state for progress bar message
    if(!quiet) cat('\n')
  },

  # open a swat config file
  open_config_file = function(f, output=TRUE, refresh=FALSE, update_stats=refresh) {

    # check if the file is already loaded and skip when refresh not requested
    needs_loading = refresh | !get_cio_df(what='loaded', f=f, drop=TRUE)
    if( length(needs_loading) == 0L ) stop('file name not recognized')
    if( needs_loading )
    {
      # recover from load errors
      load_result = tryCatch({

        # open and parse the file (overwrites line_df_temp and stor_df[[f]])
        get_config_tables(f, output=FALSE, refresh=refresh)

      }, error = function(err) err)

      # copy errors when there is a problem
      is_new = which(cio_df[['file']] == f)
      if( is(load_result, 'error') )
      {
        # copy the error message to the files data frame and flag as error
        cio_df[['msg']][is_new] <<- as.character(load_result)
        cio_df[['error']][is_new] <<- TRUE

      } else {

        # set the 'loaded' flag in files metadata and directory list
        cio_df[['loaded']][is_new] <<- TRUE

        # add type and group columns to temporary variables data frame
        line_df_temp[['type']] <<- cio_df[['type']][is_new]
        line_df_temp[['group']] <<- cio_df[['group']][is_new]

        # copy contents of temporary data frame to the persistent
        line_df <<- rbind(subset(line_df, file != f), line_df_temp)
        rownames(line_df) <<- seq(nrow(line_df))

        # parse file.cio to update groups
        if(f == 'file.cio') parse_file_cio(refresh=FALSE)

        # update OS file info
        if(update_stats) stats_cio_df()
      }
    }

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
    read_path = get_cio_df(what='path', f=f, drop=TRUE)
    msg_refresh = 'file not recognized. Try refreshing the project directory with refresh_cio_df'
    if( length(read_path) == 0 ) stop(msg_refresh)

    # count all available table number(s) then load tables in a loop
    table_num = line_df_temp[['table']] |> unique(na.rm=TRUE) |> sort()
    stor_df[[f]] <<- lapply(table_num, \(tn) {
      rswat_rtable_txt(line_df_temp, read_path, table_num=tn, quiet=TRUE)
    })

    # workarounds for header-less tables from weather-wgn.cli and weather input files
    if(f == 'weather-wgn.cli') table_num = fix_weather_gen()
    if('weather' == get_cio_df(what='type', f=f, drop=TRUE)) table_num = fix_weather_input(f)

    # workaround for loading output tables with missing/mismatched units row
    if( 'output' == get_cio_df(what='type', f=f, drop=TRUE) ) { fix_output_table(f) } else {

      # copy variable names for fread results, in a loop over table numbers
      for(tn in table_num)
      {
        # repair ragged table data where one or more headers are missing
        line_df_temp <<- rswat_fix_config(line_df_temp, table_num=tn)

        # initialize column names using those assigned by data.table::fread
        nm_head = names(stor_df[[f]][[tn]])

        # find relevant rows of temporary metadata list
        is_table = line_df_temp[['table']] == tn
        is_head = is_table & line_df_temp[['header']]
        if( any(is_head) )
        {
          # describe the problem when there is a mismatch
          msg_mismatch = paste(sum(is_head), 'column names, but ncol=', length(nm_head))
          if(length(nm_head) != sum(is_head)) stop(msg_mismatch)

          # replace column names with any header names already known to rswat
          nm_head = line_df_temp[['string']][is_head]
          names(stor_df[[f]][[tn]]) <<- nm_head
        }

        # check for mismatch in data rows max field number
        n_field_extra = max(line_df_temp[['field_num']][is_table]) - length(nm_head)
        msg_mismatch = paste('data row(s) had', n_field_extra, 'extra (unnamed) fields')
        if( n_field_extra > 0L ) stop(msg_mismatch)

        # gather classes and precision levels of column
        nm_class = unlist( lapply(stor_df[[f]][[tn]], class) )
        n_prec = line_df_temp[is_table & !is_head,, drop=FALSE] |>
          dplyr::group_by(field_num) |>
          dplyr::summarize(n_prec = ifelse(all(is.na(n_prec)), NA, max(n_prec, na.rm=TRUE)) ) |>
          dplyr::arrange(field_num) |>
          dplyr::pull(n_prec)

        # propagate the variable names, classes, n_prec to all rows
        line_df_temp[is_table,] <<- line_df_temp[is_table,, drop=FALSE] |>
          dplyr::mutate(name = nm_head[field_num]) |>
          dplyr::mutate(class = nm_class[field_num]) |>
          dplyr::mutate(n_prec = n_prec[field_num])
      }
    }

    # return the result from storage on request
    if(output) return(stor_df[[f]])
  },

  # get data frame summarizing the text field data, copying comment to cio_df
  parse_config_txt = function(f=NULL, output=TRUE) {

    # return an empty data frame with named columns when no f supplied
    if(is.null(f)) return(rswat_scan_txt())

    # load plain text fields and build a data frame of info about them
    type = get_cio_df(what='type', f=f, drop=TRUE)
    line_df_temp <<- get_config_txt(f) |> rswat_scan_txt(f, type=type) |> rswat_ftable_txt()

    # if the data frame is empty (empty file), then we are done
    if(nrow(line_df_temp) == 0L) return(line_df_temp)

    # assign precision
    line_df_temp <<- rswat_n_prec_txt(line_df_temp, quiet=TRUE)

    # copy comment (if any) before returning
    if(f %in% names(txt)) cio_df[['msg']][ which(cio_df[['file']] == f) ] <<- txt[[f]][[1]]
    if(output) return(line_df_temp)
    return(invisible())
  },

  # load plain text as a list of character strings (one per line)
  get_config_txt = function(f, n_line=Inf, output=TRUE) {

    # `f` must be listed in `cio_df`. Grab the file path from there
    msg_refresh = paste('file', f, 'not found. Try calling rswat() again to scan for new files')
    idx_cio = which( cio_df[['file']] == f )
    if( length(idx_cio) == 0L ) stop(msg_refresh)
    read_path = cio_df[['path']][idx_cio]
    if( is.na(read_path) ) stop(msg_refresh)

    # calculate the file hash and compare with previous (if any)
    new_hash = rlang::hash_file(read_path)
    old_hash = cio_df[['hash_load']][idx_cio]
    if( length(old_hash) == 0L ) old_hash = NA
    is_new = ifelse(is.na(old_hash), TRUE, new_hash==old_hash)

    # skip loading if cached data available and file hash has not changed
    if( !(f %in% names(txt)) | is_new )
    {
      # set number of lines to read with default -1 indicating all
      n_readLines = -1L

      # exceptions for weather and output (only their header lines are loaded by this method)
      if( cio_df[['type']][idx_cio] == 'weather' ) n_readLines = .rswat_gv_line_num('weather') - 1L
      if( cio_df[['type']][idx_cio] == 'output' ) n_readLines = .rswat_gv_line_num('output', f) - 1L

      # read line-by-line text from disk and update file properties
      txt[[f]] <<- readLines(read_path, n=n_readLines)
      cio_df[['hash_load']][idx_cio] <<- new_hash
      cio_df[['time_load']][idx_cio] <<- Sys.time()
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
    unknown_files = cio_group[['file']][ !( cio_group[['file']] %in% existing_files ) ]
    if( length(unknown_files) > 0 )
    {
      # fill in file info flagging as non-existent
      unknown_df = data.frame(file = unknown_files,
                              path = file.path(swat_dir, unknown_files),
                              exists = FALSE,
                              loaded = FALSE,
                              error = TRUE,
                              msg = 'Error: file was listed in file.cio but not found on disk')

      # merge with existing directory info
      cio_df <<- unknown_df |> dplyr::full_join(cio_df, by=names(unknown_df))
    }

    # write group names to known files in directory listing
    is_listed = cio_df[['file']] %in% cio_group[['file']]#[!is_unknown]
    idx_group = cio_df[['file']][is_listed] |> match(cio_group[['file']])
    cio_df[['group']][is_listed] <<- cio_group[['group']][idx_group]

    # update flags
    is_cio = cio_df[['file']] == 'file.cio'
    cio_df[['group']][ is_cio ] <<- 'cio'
    cio_df[['loaded']][ is_cio ] <<- TRUE
    cio_df[['type']][ is_listed | is_cio ] <<- 'config'

    # set factor levels for groups with cio first, then those listed in file.cio, then others
    group_order = unique( c('cio', cio_group[['group']], cio_df[['group']]) )

    # reorder the rows so they match the group order in file.cio
    idx_reorder = cio_df[['group']] |> factor(levels=group_order) |> order()
    cio_df <<- cio_df[idx_reorder,]
    rownames(cio_df) <<- seq(nrow(cio_df))
  },

  # hack to present weather generator as a set of station tables plus a single locations table
  fix_weather_gen = function() {

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
  },

  # hack to present weather input data files as pairs of data frames
  fix_weather_input = function(f) {

    # this introduces a second table to hold weather data
    table_num = 2L

    # identify group for the file
    group = strsplit(basename(f), split='\\.')[[1L]][2L]
    read_path = get_cio_df(what='path', f=f, drop=TRUE)

    # hard-coded weather input file variable name, class, and precision
    df_start = data.frame(field_num = seq(3L),
                          name = c('year', 'jday', group),
                          class = c('integer', 'integer', 'numeric'),
                          string = '',
                          file = f,
                          type = 'weather',
                          group = group,
                          table = table_num,
                          header = FALSE,
                          tabular = TRUE,
                          skipped = FALSE,
                          n_prec = c(NA, NA, 5L),
                          line_num = .rswat_gv_line_num('weather') )

    # temperature has two columns of data
    if(group == 'tmp')
    {
      df_start[3L, 'name'] = 'tmp_min'
      df_add = utils::modifyList(df_start[3L,], data.frame(field_num=4L, name='tmp_max'))
      df_start = rbind(df_start, df_add)
    }

    # read the table with fread
    stor_df[[f]][[table_num]] <<- rswat_rtable_txt(line_df = df_start,
                                                   txt_path = read_path,
                                                   table_num = table_num,
                                                   all_rows = TRUE)

    # find the final line number in the file
    last_line_num = .rswat_gv_line_num('weather') + nrow( stor_df[[f]][[table_num]] ) - 1L

    # set names and replace NAs
    names(stor_df[[f]][[table_num]]) <<- df_start[['name']]
    stor_df[[f]][[table_num]][ stor_df[[f]][[table_num]] == .rswat_gv_weather_NA_val() ] <<- NA

    # add rows to df_start to represent last row of data
    df_end = utils::modifyList(df_start, data.frame(line_num=last_line_num))
    line_df_temp <<- rbind(line_df_temp, df_start, df_end)

    # return the new set of table numbers
    return(seq(2L))
  },

  # hack to match headers, units, and data in output files
  fix_output_table = function(f) {

    # this replaces the existing table (which has header names only)
    table_num = 1L
    f_path = get_cio_df(what='path', f=f, drop=TRUE)
    line_num_start = .rswat_gv_line_num('output', f)

    # TODO: add group names?

    # identify non-header lines that were accidentally scanned
    is_header = split(line_df_temp[['class']], line_df_temp[['line_num']]) |>
      sapply(\(x) all(x=='character'))

    # copy the header rows and modify fields for rtable call below
    line_num_min = min(line_df_temp[['line_num']])
    df_header = line_df_temp[line_df_temp[['line_num']] == line_num_min, ]
    df_start = df_header |>
      utils::modifyList(list(header = FALSE,
                             tabular = TRUE,
                             class = 'numeric',
                             line_num = line_num_start - sum(!is_header)))

    # read the table starting from the first data row (quiet suppresses empty file warnings)
    stor_df[[f]][[table_num]] <<- rswat_rtable_txt(line_df = df_start,
                                                   txt_path = f_path,
                                                   table_num = table_num,
                                                   all_rows = TRUE,
                                                   quiet = TRUE)

    # find known column names, treating all but first of any set of duplicates as unknown
    is_unnamed = nchar(df_start[['string']]) == 0L
    is_duplicated = duplicated(df_start[['string']][!is_unnamed])
    if( any(!is_unnamed) ) is_unnamed[ which(!is_unnamed)[is_duplicated] ] = TRUE

    # return from empty file (headers only) case
    if( nrow( stor_df[[f]][[table_num]] ) == 0L )
    {
      # make sure header names get copied to line_df in storage
      if( any(!is_unnamed) ) df_header[['name']][!is_unnamed] = df_start[['string']][!is_unnamed]
      if( any(is_unnamed) ) df_header[['name']][is_unnamed] = paste0('V', which(is_unnamed))
      line_df_temp <<- df_header

      # write an empty data frame to storage with the expected column names
      stor_df[[f]][[table_num]] <<- rswat_empty_df(df_header[['name']])
      return(invisible())
    }

    # find the final line number in the file
    last_line_num = line_num_start - sum(!is_header) + nrow( stor_df[[f]][[table_num]] )
    df_end = rbind(df_start, utils::modifyList(df_start, data.frame(line_num=last_line_num)))

    # assign names and classes where they are missing
    names(stor_df[[f]][[table_num]])[!is_unnamed] <<- df_start[['string']][!is_unnamed]
    nm_class = unlist( lapply(stor_df[[f]][[table_num]], class) )
    df_end[['name']] = names(stor_df[[f]][[table_num]])
    df_end[['class']] = nm_class

    # repair line_df_temp
    df_header[['name']] = names(stor_df[[f]][[table_num]])
    df_header[['class']] = nm_class
    line_df_temp <<- rbind(df_header, df_end)

    # TODO: check if we need to replace NAs like we do with weather files?
    #stor_df[[f]][[table_num]][ stor_df[[f]][[table_num]] == .rswat_gv_weather_NA_val() ] <<- NA
  }

))
