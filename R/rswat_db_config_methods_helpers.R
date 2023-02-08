
#' Scan a SWAT+ project directory
#'
#' Looks for files in the supplied `swat_dir` using `base::list.files(swat_dir)` and
#' builds a data frame listing them, along with their size and date modified (as reported
#' by the OS), and some internal flags.
#'
#' An existing data frame, `cio_df`, can be supplied to merge an existing list of files
#' (not found on disk, or in a different directory). All files are labelled according to
#' known SWAT+ file extension patterns.
#'
#' Alternatively, supply a list of file names in `f` to skip both the `list.files` call
#' and the OS file info calls that rely on paths. This is useful for classifying arbitrary
#' lists of files with unspecified locations.
#'
#' @param swat_dir character, the path to the directory containing SWAT+ config files
#' @param cio_df data frame, structured like the output of `rswat_scan_dir()`
#' @param f character vector, file names to classify (instead of those found in `swat_dir`)
#'
#' @return a data frame with a list of file info
#' @export
rswat_scan_dir = function(swat_dir=NULL, cio_df=NULL, f=NULL)
{
  # set up default column names and types
  if( is.null(cio_df) )
  {
    cio_df = data.frame(file = character(0), # file name
                        type = character(0), # category: one of the names in `type_lu`
                        group = character(0), # group (as listed in file.cio)
                        size = numeric(0), # size of the file on disk
                        modified = .POSIXct(integer(0)), # last modification date/time (from OS)
                        exists = logical(0), # is the file found in your swat directory?
                        loaded = logical(0), # has the file been loaded by rswat?
                        known = logical(0), # is the file known to file.cio?
                        n_line = integer(0), # number of lines in the file
                        n_var = integer(0), # number of variables identified in the file
                        n_table = integer(0), # number of tables identified in the file
                        n_skip = integer(0), # number of lines skipped in parsing the file
                        msg = character(0), # the first line of the file (a comment)
                        path = character(0), # the absolute path to the file
                        error = logical(0), # indicates that a load attempt failed
                        time_load = .POSIXct(integer(0)), # the Sys.time() when file was loaded
                        hash_load = character(0)) # the file hash at the time of loading
  }

  # if no directory supplied, we are finished
  if( is.null(swat_dir) ) return(cio_df)

  # scan for files on request
  all_files = f
  is_scanned = is.null(all_files)
  if(is_scanned) all_files = list.files(swat_dir, all.files=TRUE, no..=TRUE)

  # join with any previous results
  cio_new = data.frame(file=all_files) |>
    dplyr::mutate( known = FALSE ) |>
    dplyr::mutate( loaded = FALSE ) |>
    dplyr::mutate( error = FALSE ) |>
    dplyr::mutate( type = 'unknown' ) |>
    dplyr::mutate( path = file.path(swat_dir, file) ) |>
    dplyr::anti_join( cio_df, by=c('file') ) |>
    dplyr::full_join( cio_df, by=c('file', 'known', 'loaded', 'error', 'type', 'path') ) |>
    dplyr::mutate( exists = file.exists(path) )

  # add the size and time modified for each file on disk
  if(is_scanned)
  {
    finfo = file.info(cio_new[['path']])
    cio_new[['size']] = units::set_units(units::set_units(finfo[['size']], bytes), kilobytes)
    cio_new[['modified']] = finfo[['mtime']]
  }

  # tags for SWAT+ file type
  type_lu = .rswat_gv_type_lu()
  type_match = lapply(type_lu[['pattern']], \(s) which(grepl(s, cio_new[['file']])) )
  for( j in seq(nrow(type_lu)) ) cio_new[['type']][ type_match[[j]] ] = type_lu[['type']][j]
  cio_new[['known']] = cio_new[['type']] != 'unknown'

  # groups for weather files
  is_weather = cio_new[['type']] %in% 'weather'
  weather_group_list = strsplit(basename(cio_new[['file']][is_weather]), split='\\.')
  cio_new[['group']][is_weather] = sapply(weather_group_list, \(x) x[[2L]])
  return(cio_new)
}


#' Scan data values from a SWAT+ config file line-by-line
#'
#' This builds a data frame information about individual fields detected in the file.
#' These should be delimited by the character `delim` (usually whitespace). The output
#' numbers the fields on each line, detects their types, and copies the field itself as
#' a string with outer white-space removed.
#'
#' Other attributes in the output data frame are initialized here but filled in by
#' other functions (`rswat_ftable_txt` and `rswat` methods).
#'
#' If `txt` and `f` are omitted the function returns a template table with column names
#' and types assigned, but with 0 rows.
#'
#' Note that the first line of a SWAT+ config file is assumed to be a comment.
#'
#' @param txt character vector, the output of `readLines(f)` (or a subset)
#' @param f character, the file name associated with the txt data
#' @param delim character, regex for separator between fields (default is any whitespace)
#'
#' @return a data frame of information about the fields in the file
#' @export
rswat_scan_txt = function(txt=NULL, f=NULL, type='config', delim='\\s+')
{
  # TODO: check if we really need argument f or NULL defaults

  # define attribute names and types
  line_df = data.frame(file = character(0), # file name
                       type = character(0), # file type
                       group = character(0), # file group
                       string = character(0), # text string in the field (no whitespace)
                       name = character(0), # name of the associated variable
                       line_num = integer(0), # line number of field
                       field_num = integer(0), # the field number
                       skipped = logical(0), # was the field skipped in parsing? ** DO WE NEED THIS?
                       class = character(0), # class of the field
                       header = logical(0), # does the field appear to lie on a header row?
                       tabular = logical(0), # does the field appear to be tabular data?
                       table = integer(0), # table number for files with multiple tables
                       n_prec = integer(0)) # number of digits after decimal point

  # return this data frame in default calls without arguments (or whenever txt is NULL)
  n_txt = length(txt)
  if(n_txt == 0L) return(line_df)

  # set the number of lines to skip and check for all skipped case
  n_skip = .rswat_gv_line_num(type, f, skip=TRUE)
  if( min(n_txt, n_txt-n_skip) <= 0L ) return(line_df)
  not_skipped = if(n_skip==0L) seq_along(txt) else -seq(n_skip)

  # strip comment then split into white space delimited fields to get columns
  txt_wsr = txt[not_skipped] |> trimws() |> strsplit(split=delim)
  txt_cnum = lapply(txt_wsr, seq_along)
  line_num_add = if(n_txt > 1L) seq(n_txt-1L) else 0L
  txt_ln = Map(\(x, y) rep(x, length(y)), x=n_skip+line_num_add, y=txt_cnum)

  # bundle everything into a data frame, adding back unassigned columns from template
  nm_join = c('string', 'line_num', 'field_num')
  line_df = data.frame(string = unlist(txt_wsr),
                       line_num = unlist(txt_ln),
                       field_num = unlist(txt_cnum)) |> dplyr::left_join(line_df, by=nm_join)

  # handle files that encode Boolean as y/n in one or more tables
  if( f %in% .rswat_gv_logical_as_yn() )
  {
    # translate 'y'/'n' as TRUE/FALSE
    is_logi = line_df[['string']] %in% c('n', 'y')
    is_logi[is_logi=='y'] = TRUE
    is_logi[is_logi=='n'] = FALSE

  } else { is_logi = rep(FALSE, nrow(line_df)) }

  # replace 'null' with empty character
  line_df[['string']][ grepl('null', line_df[['string']], fixed=TRUE) ] = ''

  # detect numeric and integer using R's built-in interpreter, else label as character
  is_num = !is.na( suppressWarnings( as.numeric(line_df[['string']]) ) ) & !is_logi
  is_int = is_num & !grepl('.', line_df[['string']], fixed=TRUE)
  is_num = is_num & !is_int
  is_char = !is_int & !is_num & !is_logi

  # vector of classes for the input
  available_classes = c('logical', 'character', 'numeric', 'integer')
  line_df[['class']] = available_classes[apply(cbind(is_logi, is_char, is_num, is_int), 1, which)]

  # identify all-character lines
  line_num_unique = unique(line_df[['line_num']])
  header_byln = sapply(split(line_df[['class']], line_df[['line_num']]), \(x) all(x=='character'))
  line_df[['header']] = line_df[['line_num']] %in% line_num_unique[header_byln]

  # add filename attribute and return
  line_df[['file']] = f
  return(line_df)
}


#' Identify tables in SWAT+ config text
#'
#' This function splits the line numbers of a plain text file into blocks representing
#' tables of data. All-character lines are identified as headers, and all subsequent
#' non-header lines are assumed to belong to the preceding header.
#'
#' This rule is applied iteratively starting with the first header line. The one exception
#' is with multiple contiguous lines of all character fields. These are assumed to comprise
#' one single table. The following columns in line_df are updated:
#'
#'  'tabular' logical, indicates the field is part of a data row in a table
#'  'table' integer, the table number
#'  'skipped' logical, indicating that the line for this field was not assigned to a table
#'
#' @param line_df data frame of the form returned by `rswat_scan_txt`
#'
#' @return a copy of `line_df` with updated attributes labeling tables
#' @export
rswat_ftable_txt = function(line_df)
{
  # return empty tables unchanged
  n_lines = nrow(line_df)
  if( n_lines == 0 ) return(line_df)

  # find header line numbers and initialize table column
  ln_head = line_df[['line_num']][ line_df[['header']] ] |> unique()
  line_df[['table']] = rep(NA, n_lines)

  # deal with all-character tables
  idx_header_pair = c(FALSE, diff(ln_head) == 1)
  if( any(idx_header_pair) )
  {
    # relabel misidentified header lines
    line_df[['header']][ line_df[['line_num']] %in% ln_head[idx_header_pair] ] = FALSE
    ln_head = line_df[['line_num']][ line_df[['header']] ] |> unique()
  }

  # "file.cio" is a special case (no headers)
  if( any(line_df[['file']] == 'file.cio') )
  {
    line_df[['header']] = FALSE
    line_df[['tabular']] = TRUE
    line_df[['skipped']] = FALSE
    line_df[['table']] = 1

  } else {

    # identify header lines as breakpoints defining blocks
    n_block = length(ln_head)
    ln_breaks = c(ln_head, max(line_df[['line_num']]) + 1L)
    ln_block = seq(n_block) |> lapply(\(i) c(ln_breaks[i]:(ln_breaks[i+1L] - 1L)) )

    # indices and flag to indicate table data lying below headers
    ln_tabular = ln_block |> lapply(\(ln) ln[-1L]) |> unlist()
    line_df[['tabular']] = line_df[['line_num']] %in% ln_tabular

    # processing by blocks (if there are any)
    line_df[['skipped']] = !( line_df[['header']] | line_df[['tabular']] )
    if( !all(line_df[['skipped']]) )
    {
      # by block, list unique line numbers and find number of fields per line
      ln_unique = unique(line_df[['line_num']])
      n_byln = sapply(ln_unique, \(ln) sum(line_df[['line_num']] == ln))
      n_fields_byblock = lapply(ln_block, \(ln) n_byln[ln_unique %in% ln])

      # by block, assign table numbers
      table_num_byblock = Map(\(i, nf) rep(i, sum(nf)), nf=n_fields_byblock, i=seq(n_block))
      line_df[['table']][ !line_df[['skipped']] ] = unlist(table_num_byblock)
    }
  }

  # fix for the unusual table structure in weather-wgn.cli
  if( any(line_df[['file']] == 'weather-wgn.cli') )
  {
    # there is one table per row of the station info table
    ln_info = ln_head - 1L
    idx_ln_info = seq_along(ln_info)
    for(i in idx_ln_info) line_df[['table']][ line_df[['line_num']] %in% ln_info[i] ] = -i
  }

  return(line_df)
}


#' Extract a table from SWAT+ config text
#'
#' Load a table from a SWAT+ file via `data.table::fread` and return it as a data frame.
#'
#' @param line_df data frame of the form returned by `rswat_ftable_txt`
#' @param txt_path character, the path to the SWAT+ text file to be read
#' @param table_num integer, the index of the table in the file
#' @param all_rows logical, loads all rows (using line_df only to determine start line)
#' @param quiet logical, suppresses warnings about errors in fread
#'
#' @return data frame, the specified parameter table in the file
#' @export
rswat_rtable_txt = function(line_df, txt_path, table_num=NULL, all_rows=FALSE, quiet=FALSE)
{
  # extract SWAT+ dir and file name from path and check for y/n logical encoding
  swat_dir = dirname(txt_path)
  fname = basename(txt_path)
  has_yn = fname %in% .rswat_gv_logical_as_yn()

  # check for mismatch in path and loaded data
  if( any(line_df[['file']] != fname) ) stop('mismatch between line_df and txt_path')

  # count the tables in the file
  table_num_vec = line_df[['table']]
  all_num = table_num_vec |> unique(na.rm=TRUE) |> sort()

  # catch problems with table_num
  all_num_string = paste(all_num, collapse=', ')
  if( length(table_num) > 1L ) stop('table_num must be an integer or NULL')
  if( !(table_num %in% all_num) )
  {
    if(length(all_num) > 1L) stop(paste('table_num should be one of:', all_num_string))
    stop(paste('table_num must be set to', all_num_string))
  }

  # find index for this table in line_df and copy the required subset
  is_row_requested = !is.na(table_num_vec) & (table_num_vec == table_num)
  nm_needed = c('string', 'header', 'line_num', 'field_num', 'class', 'table')
  line_df_sub = line_df[is_row_requested, nm_needed, drop=FALSE]
  is_head = line_df_sub[['header']]

  # identify the line numbers to load and expected number of columns
  ln_start = min(line_df_sub[['line_num']])
  ln_read = ln_start:max(line_df_sub[['line_num']])
  nrow_load = ifelse(all_rows, Inf, length(ln_read))
  n_field = max( line_df_sub[['field_num']] )

  # an empty data frame to return when there is no table data
  df_empty = rswat_empty_df(n_field)
  if( any(is_head) )
  {
    # add names to empty output
    df_empty = rswat_empty_df( line_df_sub[['string']][is_head] )

    # if there are unnamed columns, skip loading header row
    n_head = max( line_df_sub[['field_num']][is_head] )
    if(n_head < n_field)
    {
      ln_start = ln_start + 1L
      nrow_load = nrow_load - 1L
      is_head = FALSE
    }
  }

  # empty config files
  if( nrow_load == 0L ) return(df_empty)

  # check for unexpectedly empty files when attempting to read all
  if( is.infinite(nrow_load) )
  {
    # fast call to check if there is at least one data row
    is_empty = 1L == nrow(data.table::fread(input = txt_path,
                                            skip = ln_start-2L,
                                            nrows = 2L,
                                            header = FALSE,
                                            fill = TRUE))

    # in this case skip would equal length of file, causing an error
    if(is_empty) return(df_empty)
  }

  # load all requested table lines with fread (note interaction of `nrows` and `header` args)
  df_out = data.table::fread(input = txt_path,
                             skip = ln_start-1L,
                             header = any(is_head),
                             select = seq(n_field),
                             nrows = nrow_load - any(is_head),
                             fill = TRUE) |> data.frame()

  # assign attributes describing where this table can be found on disk
  attr(df_out, 'rswat_path') = swat_dir
  attr(df_out, 'rswat_fname') = fname
  attr(df_out, 'rswat_table_num') = table_num

  # tidy non-empty tables
  if( nrow(df_out) > 0L )
  {
    # convert 'y'/'n' columns to logical
    is_logical = line_df_sub[['class']] == 'logical'
    if( any(is_logical) )
    {
      col_logical = line_df_sub[['field_num']][is_logical] |> unique()
      df_out[, col_logical] = df_out[, col_logical] == 'y'
    }
  }

  return(df_out)
}

#' Determine precision of numeric columns in a SWAT+ config file
#'
#' Determines a the number of digits to appear after the decimal point for numeric types
#' in columns of a SWAT+ table, based on the existing printed values.
#'
#' If `table_num=NULL`, the function processes all tables.
#'
#' Most of the time this works without issue on the output of `rswat_ftable_txt`, but
#' there are certain exceptions to deal with. These exceptions are handled by modifying
#' `line_df` after the `rswat_ftable_txt` call but prior to this function call (this
#' happens in `rswat` methods ...)
#'
#' @param line_df data frame of the form returned by `rswat_ftable_txt`
#' @param table_num integer, the table number to load (or `NULL` to load all)
#' @param quiet logical, indicates to suppress console messages
#'
#' @return data frame, a copy of `line_df` with the 'n_prec' column updated
#' @export
rswat_n_prec_txt = function(line_df, table_num=NULL, quiet=FALSE)
{
  # find all available table numbers
  table_num_vec = line_df[['table']]
  all_num = table_num_vec |> unique(na.rm=TRUE) |> sort()
  if( is.null(table_num) )
  {
    # recursive call to return all tables in a list if no table number specified
    rswat_nprec_result = lapply(all_num, \(tn) {
      rswat_n_prec_txt(subset(line_df, table==tn), tn, quiet)
    })
    return(do.call(rbind, rswat_nprec_result))
  }

  # catch problems with table_num
  all_num_string = paste(all_num, collapse=', ')
  if( length(table_num) > 1 ) stop('table_num must be an integer or NULL')
  if( !(table_num %in% all_num) )
  {
    if(length(all_num) > 1) stop(paste('table_num should be one of:', all_num_string))
    stop(paste('table_num must be set to', all_num_string))
  }

  # find the requested table info in line_df
  idx_row_requested = which( !is.na(table_num_vec) & (table_num_vec == table_num) )
  nm_needed = c('string', 'line_num', 'field_num', 'table', 'class', 'n_prec')

  # identify numeric fields. If there are none, we are done
  is_num = line_df[['class']][idx_row_requested] == 'numeric'
  if( sum(is_num) == 0 ) return(line_df)

  # copy the relevant subset of the lines info data frame
  linedf_sub = line_df[idx_row_requested[is_num], nm_needed]

  # split field strings by field number (ie column)
  string_bycol = split(linedf_sub[['string']], linedf_sub[['field_num']])
  n_bycol = sapply(string_bycol, length)

  # count digits after the dot
  decimal_bycol = lapply(string_bycol, \(s) gregexpr('\\.[0-9]+', s, perl=T))
  n_prec_bycol = lapply(decimal_bycol, \(d) sapply(d, \(m) attr(m, 'match.length') - 1 ))

  # use maximum precision in case of inconsistencies within a column (and warn user)
  is_consistent = sapply(n_prec_bycol, \(n) all(n == max(n)))
  if( any(!is_consistent) )
  {
    n_prec_bycol = lapply(n_prec_bycol, \(n) rep(max(n), length(n)))
    f = line_df[['file']][1]
    msg_file = paste0('in file ', f, ', table ', table_num, ':')
    if(!quiet) warning(paste(msg_file, 'inconsistent precision within columns (using maximum)'))
  }

  # write the n_prec attribute and finish
  line_df[['n_prec']][idx_row_requested[is_num]] = unlist(n_prec_bycol)
  return(line_df)
}


#' Add rows to line_df to represent missing headers in SWAT+ config file
#'
#' @param line_df data frame of the form returned by `rswat_ftable_txt`
#' @param table_num integer, the index of the table in the file
#'
#' @return a copy of line_df, possibly with rows added
#' @export
rswat_fix_config = function(line_df, table_num=1L)
{
  # count number of fields in header row and return from degenerate cases
  is_table = line_df[['table']] == table_num
  line_df_sub = line_df[is_table, , drop=FALSE]
  is_head = line_df_sub[['header']]
  if( all(is_head) | !any(is_head) | !any(is_table) ) return(line_df)

  # count max number of fields in all rows
  n_head = line_df_sub[['field_num']][is_head] |> max()
  n_add = max(line_df_sub[['field_num']]) - n_head

  # if there are no missing headers, return the input, else make new headers
  if(n_add == 0L) return(line_df)
  nm_add = tail(paste0('V', seq(n_head + n_add)), n_add)

  # copy the last header field info to fill in the missing rows
  idx_clone = which(is_head)[ which.max( line_df_sub[['field_num']][is_head] ) ]
  row_clone = line_df_sub[idx_clone, , drop=FALSE]
  row_clone = row_clone[rep(1L, n_add), ] |>
    utils::modifyList(list(string = nm_add,
                           field_num = row_clone[['field_num']] + seq(n_add)))

  # append the new rows and reorder
  line_df_out = rbind(line_df, row_clone) |> dplyr::arrange(line_num)
  rownames(line_df_out) = NULL
  return(line_df_out)
}





