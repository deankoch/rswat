#' Validity check for a directory
#'
#' Helper function to check if a directory string points to an existing location.
#'
#' Returns its first argument `d` if the directory is found on disk. If the directory doesn't
#' exist, or if `d` is not a valid directory string, the function throws an error. The only
#' exception is `d=NA` (allowing the directory to be unassigned) which returns `NA`
#'
#' @param d character, the directory path to check
#' @param vname character, name to use in reference to `d` (for error messages)
#'
#' @return logical, indicating if the directory has been assigned
#' @export
rswat_validate_dpath = function(d, d_name='d') {

  if( is.na(d) ) return(d)
  if( !is.character(d) ) stop(paste('directory', d_name, 'must be a character string'))
  d = normalizePath(d, winslash='/')
  if( !dir.exists(d) ) stop(paste0('directory "', d, '" not found on disk'))
  return(d)
}

#
#' Validity check for a file path
#'
#' Helper function to check if a file path string points to an existing file with the given extension.
#'
#' If a file with the expected extension is found on disk at the path `p`, the function returns `p`.
#' Otherwise it throws an error. The only exception is `p=NA` (allowing the path to be unassigned),
#' which returns `NA`
#'
#' Set `extension=NA` to allow any file extension.
#'
#' @param p character, the file path to check
#' @param extension character, the file extension to expect including the period (eg '.exe' or '.txt')
#' @param p_name character, name to use in reference to `p` (for error messages)
#'
#' @return logical, indicating if the directory has been assigned
#' @export
rswat_validate_fpath = function(p, extension=NA, p_name='p') {

  if( is.na(p) ) return(p)
  if( !is.character(p) ) stop(paste('file path', p_name, 'must be a character string'))
  p = normalizePath(p, winslash='/')
  if( !file.exists(p) ) stop(paste0('file path "', p, '" not found on disk'))
  if( is.na(extension) ) return(p)
  if( !endsWith(p, extension) ) stop(paste0('file path "', p, '" did not have extension ', extension))
  return(p)
}


#' Scan a SWAT+ project directory
#'
#' Looks for files in the supplied `swat_dir` and builds a data frame listing them
#' along with some OS file info (size, etc) and internal flags. Files are labelled
#' according to known SWAT+ file extension patterns.
#'
#' An existing data frame, `cio_df`, can be supplied to list files not found on disk
#' alongside the ones detected by `base::list.files`
#'
#' @param swat_dir path to the directory containing SWAT+ config files
#' @param cio_df optional data frame structured like the output of `rswat_scan_dir()`
#'
#' @return a data frame with a list of file info for `swat_dir`
#' @export
rswat_scan_dir = function(swat_dir=NULL, cio_df=NULL)
{
  # set up default column names and types
  if( is.null(cio_df) )
  {
    cio_df = data.frame(file = character(0), # file name
                        type = character(0), # category: one of the names in `type_lu`
                        group = character(0), # group (as listed in file.cio)
                        size = numeric(0), # size of the file on disk
                        modified = numeric(0), # date the file was last modified on disk
                        exists = logical(0), # is the file found in your swat directory?
                        loaded = logical(0), # has the file been loaded by rswat?
                        known = logical(0), # is the file known to file.cio?
                        n_line = integer(0), # number of lines in the file
                        n_var = integer(0), # number of variables identified in the file
                        n_table = integer(0), # number of tables identified in the file
                        n_skip = integer(0), # number of lines skipped in parsing the file
                        msg = character(0), # the first line of the file (a comment)
                        path = character(0)) # the absolute path to the file
  }

  # if no directory supplied, we are finished
  if( is.null(swat_dir) ) return(cio_df)

  # scan for files then join with any previous results
  cio_new = data.frame(file=list.files(swat_dir)) |>
    dplyr::mutate( known = FALSE ) |>
    dplyr::mutate( loaded = FALSE ) |>
    dplyr::mutate( path = file.path(swat_dir, file) ) |>
    dplyr::anti_join( cio_df, by=c('file') ) |>
    dplyr::full_join( cio_df, by=c('file', 'known', 'loaded', 'path') ) |>
    dplyr::mutate( exists = file.exists(path) ) |>
    dplyr::mutate( known = known | !is.na(type) )

  # add the size and time modified for each file on disk
  finfo = file.info(cio_new[['path']])
  cio_new[['size']] = units::set_units(units::set_units(finfo[['size']], bytes), kilobytes)
  cio_new[['modified']] = finfo[['mtime']]

  # label SWAT+ file type before returning
  type_lu = .rswat_gv_type_lu()
  type_match = lapply(type_lu[['pattern']], \(s) which(grepl(s, cio_new[['file']])) )
  for( j in seq(nrow(type_lu)) ) cio_new[['type']][ type_match[[j]] ] = type_lu[['type']][j]
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
rswat_scan_txt = function(txt=NULL, f=NULL, delim='\\s+')
{
  # define attribute names and types
  line_df = data.frame(file = character(0), # file name
                       string = character(0), # text string in the field (no whitespace)
                       name = character(0), # name of the associated variable
                       line_num = integer(0), # line number of field
                       field_num = integer(0), # the field number
                       skipped = logical(0), # was the field skipped in parsing?
                       class = character(0), # class of the field
                       header = logical(0), # does the field appear to lie on a header row?
                       tabular = logical(0), # does the field appear to be tabular data?
                       table = integer(0), # table number for files with multiple tables
                       nprec = integer(0)) # number of digits after decimal point

  # if there are no non-empty lines, we are finished
  n_txt = length(txt)
  if(n_txt == 0) return(line_df)

  # strip comment then split into white space delimited fields to get columns
  txt_wsr = txt[-1] |> trimws() |> strsplit(split=delim)
  txt_cnum = lapply(txt_wsr, seq_along)
  txt_ln = Map(\(x, y) rep(x, length(y)), x=1L+seq(n_txt-1L), y=txt_cnum)

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
      table_num_byblock = Map(\(i, nf) rep(i, sum(nf)),  nf=n_fields_byblock, i=seq(n_block))
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
#'
#' @return data frame, the specified parameter table in the file
#' @export
rswat_rtable_txt = function(line_df, txt_path, table_num=NULL)
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
  if( length(table_num) > 1 ) stop('table_num must be an integer or NULL')
  if( !(table_num %in% all_num) )
  {
    if(length(all_num) > 1) stop(paste('table_num should be one of:', all_num_string))
    stop(paste('table_num must be set to', all_num_string))
  }

  # find index for this table in line_df and copy the required subset
  is_row_requested = !is.na(table_num_vec) & (table_num_vec == table_num)
  nm_needed = c('header', 'skipped', 'line_num', 'field_num', 'class', 'table')
  line_df_sub = line_df[is_row_requested, nm_needed]

  # identify the line numbers to load
  ln_start = data.table::first( line_df_sub[['line_num']] )
  ln_end = data.table::last( line_df_sub[['line_num']] )
  ln_read = ln_start:ln_end
  has_headers = any(line_df_sub[['header']])

  # attempt to load as data frame - suppress warnings and copy error messages
  df_out = tryCatch({

    # count number of data rows
    if( length(ln_read) - as.numeric(has_headers) > 0 )
    {
      # fread is preferred except in single row case
      data.table::fread(input = txt_path,
                        skip = ln_start-1,
                        nrows = length(ln_read)-as.numeric(has_headers),
                        fill = TRUE) |> as.data.frame()

    } else {

      # read table for single row case
      read.table(txt_path, skip=ln_start-as.numeric(has_headers), nrows=1)

    }

  }, error = function(err) err) |> suppressWarnings()

  # errors dealt with by returning an empty data frame with error message as attribute
  if( is(df_out, 'error') )
  {
    # report errors to user as warning
    err_msg = as.character(df_out)
    warning(err_msg)

    # dummy return value with info in attributes
    df_out = data.frame()
    attr(df_out, 'rswat_msg') = err_msg
  }

  # assign attributes describing where this table can be found on disk
  attr(df_out, 'rswat_path') = swat_dir
  attr(df_out, 'rswat_fname') = fname
  attr(df_out, 'rswat_table_num') = table_num

  # tidy non-empty tables
  if( nrow(df_out) > 0 )
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
#'@param line_df data frame of the form returned by `rswat_ftable_txt`
#'
#' @return data frame, a copy of `line_df` with the 'nprec' column updated
#' @export
rswat_nprec_txt = function(line_df, table_num=NULL)
{
  # recursive call to return all tables in a list
  table_num_vec = line_df[['table']]
  all_num = table_num_vec |> unique(na.rm=TRUE) |> sort()
  if( is.null(table_num) )
  {
    rswat_nprec_result = lapply(all_num, \(tn) rswat_nprec_txt(subset(line_df, table==tn), tn))
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
  nm_needed = c('string', 'line_num', 'field_num', 'table', 'class', 'nprec')

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
  nprec_bycol = lapply(decimal_bycol, \(d) sapply(d, \(m) attr(m, 'match.length') - 1 ))

  # use maximum precision in case of inconsistencies within a column (and warn user)
  is_consistent = sapply(nprec_bycol, \(n) all(n == max(n)))
  if( any(!is_consistent) )
  {
    nprec_bycol = lapply(nprec_bycol, \(n) rep(max(n), length(n)))
    f = line_df[['file']][1]
    msg_file = paste0('in file ', f, ', table ', table_num, ':')
    warning(paste(msg_file, 'inconsistent precision within columns (using maximum)'))
  }

  # write the nprec attribute and finish
  line_df[['nprec']][idx_row_requested[is_num]] = unlist(nprec_bycol)
  return(line_df)
}


