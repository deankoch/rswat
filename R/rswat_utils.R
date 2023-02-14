
#' Initialize an empty data frame
#'
#' Helper function to initialize a data frame of the specified size with zero rows.
#' If `nm` is a character vector, the function returns a data frame with `length(nm)`
#' named columns. If `nm` is numeric, the data frame will have `nm` unnamed columns.
#'
#' All output columns are character class.
#'
#' @param nm integer or character vector, the length or column names
#'
#' @return data frame with zero rows
rswat_empty_df = function(nm) {

  # validity checks
  if( anyNA(nm) ) stop('nm had NA value(s)')
  if( length(nm) == 0 ) stop('nm was NULL or length 0')

  # for now this just sets everything to empty character class
  if(is.character(nm)) return( stats::setNames(data.frame(matrix(nrow=0, ncol=length(nm))), nm=nm) )
  if(is.numeric(nm)) return( data.frame(matrix(nrow=0, ncol=round(head(nm,1)))) )

  stop('nm had unexpected class (it should be character of numeric)')
}


#' Return a subset of data frame columns in a specified order
#'
#' Columns of `df_unordered` named in `nm_print` are returned. `nm_print` may contain
#' names not appearing in `df_unordered`, but there should be at least one match or else
#' an empty data frame is returned without warning.
#'
#' @param df_unordered a data frame with named columns
#' @param nm_print column names to return, in order
#'
#' @return a data frame, with (possibly) reordered columns
#' @export
rswat_order_columns = function(df_unordered, nm_print) {

  # match data frame names to print names
  cols_out = match(nm_print, names(df_unordered))

  # unmatched data frame names are omitted
  cols_out = cols_out[ !is.na(cols_out) ]
  return(df_unordered[cols_out])
}



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


#' Render character data to the specified character width for printing
#'
#' Elements of `txt` shorter than `max_len` are padded with whitespace. Elements
#' the same width are unchanged, and elements longer than `max_len` are truncated.
#'
#' The default `just='left'` applies padding/truncation to the right side (end) of the
#' string; and `just='right'` does it to the left side.
#'
#' @param txt character vector
#' @param max_len integer, the maximum width allowed for printing (`NULL` to set automatically)
#' @param just character either "left" (the default) or "right"
#' @param trim logical, indicates to remove redundant whitespace and replace NAs with ~
#'
#' @return character vector, the same length as `txt`, with all elements the same width
#' @export
rswat_truncate_txt = function(txt, max_len = NULL, just = 'left', trim = TRUE) {

  # characters to indicate NAs and omitted text
  more_char = '...'
  NA_char = '~'

  # clean up input text and set default max_len to longest string length
  if(trim) txt = gsub('\\s+', ' ', replace(txt, is.na(txt), NA_char), perl=TRUE) |> trimws()
  if( is.null(max_len) ) max_len = max(nchar(txt))

  # make sure the truncation symbol is not too long
  if( max_len < nchar(more_char) ) more_char = substr(more_char, 1L, max_len)

  # widths of annotation strings
  n_NA = nchar(NA_char)
  n_more = nchar(more_char)

  # truncate then overwrite last n_more characters of truncated strings with more_char
  max_len_short = max_len - n_more
  txt_short = substr(txt, 1L, max_len) |> trimws()
  is_short = nchar(txt_short) < nchar(txt)
  txt_short[is_short] = txt[is_short] |>
    substr(1L, max_len_short) |>
    trimws() |>
    paste0(more_char)

  # pad to max_len using white-space
  n_pad = pmax(0, max_len - nchar(txt_short))
  txt_pad = sapply(n_pad, \(m) paste(rep(' ', m), collapse=''))
  if( just == 'left' ) return( paste0(txt_short, txt_pad) )
  return( paste0(txt_pad, txt_short) )
}


#' Rename columns in a data frame according to a list of aliases
#'
#' This checks the names of `d` for any matches to the strings in
#' `aliases`. If any matches are found, the column is renamed to the
#' alias, unless the alias is already in use.
#'
#' @param d a data frame to check for aliases
#' @param aliases named list of character vectors (the aliases)
#'
#' @return the data frame `d`, possibly with new names
#' @export
rswat_rename = function(d, aliases=NULL) {

  # build a look-up table from aliases (adding self references)
  alias_df = data.frame(name = do.call(c, Map(\(x, y) c(x, y), names(aliases), aliases)),
                        alias = do.call(c, Map(\(x, y) rep(x, 1L+length(y)), names(aliases), aliases)))

  # match to column names (returning input in degenerate cases)
  nm_d = names(d)
  if(is.null(d)) return(d)
  idx_match = match(nm_d, alias_df[['name']])
  is_mapped = !is.na(idx_match)
  if( !any(is_mapped) ) return(d)


  nm_new = alias_df[['alias']][ idx_match[is_mapped] ]
  is_dupe = duplicated(nm_new)
  if( any(is_dupe) )
  {
    idx_match[is_dupe] = NA
    is_mapped = !is.na(idx_match)
    if( !any(is_mapped) ) return(d)
  }

  # rename the columns
  names(d)[which(is_mapped)] = nm_new[!is_dupe]
  return(d)
}



#' Make a vector of messages for reporting progress in console via `base::cat`
#'
#' This takes a vector of object names and returns a short message for each one,
#' consisting of a percentage (how far through the list are we) followed by the
#' name.
#'
#' Strings are truncated/padded to `n_max` (by default, the console width as given
#' by the R option 'width') and each begins with the special symbol '\r', which
#' (when passed to `cat`) asks the terminal or console in which R is running to
#' do a carriage return and overwrite the existing line.
#'
#'
#' @param nm character vector, the objects to progress through
#' @param n_max integer or NULL, the maximum line width for printing
#'
#' @return a character vector the same length as `nm`
#' @export
rwat_progress = function(nm, n_max=NULL) {

  if( is.null(n_max) ) n_max = ifelse(is.null(getOption('width')), 80L, getOption('width'))
  msg_percent = paste0(round( 100 * seq_along(nm) / length(nm) ), '%')

  # \r indicates to return to beginning and overwrite existing line
  msg_progress = paste('\r[', rswat_truncate_txt(msg_percent), ']',
                       rswat_truncate_txt(nm, n_max - max(nchar(msg_percent)) - 1L))

  # return as named vector
  names(msg_progress) = nm
  return(msg_progress)
}


#' Convert between two representations of date
#'
#' If `d` is a Date vector, the function returns a data frame with the same number of rows,
#' and columns Julian date ('jday' ) and year ('year'). Otherwise the function assumes `d`
#' is a data frame (or matrix, or list) containing a 'year' column and a 'jday' or 'month'
#' column (or both). It appends a new column 'date' (or overwrites the existing one) with
#' these values converted to R Dates.
#'
#' The 'year', 'jday', and 'month' columns can also be named any of the aliases listed in
#' `.rswat_gv_date_aliases()`. When 'month' but not 'jday' is specified, the function
#' sets the date to the first of the month. When only 'year' is specified, the function
#' assigns Jan 1 of that year.
#'
#' @param d either a Date vector or dataframe, whichever `d` is not
#'
#' @return either a Date vector or dataframe, whichever `d` is not
#' @export
#'
#' @examples
#' # convert from integers to Dates
#' n_test = 10
#' date_as_int = lapply(seq(n_test), \(x) c(jday=sample.int(365, 1), year=1950 + sample.int(100, 1)))
#' date_result = rswat_date_conversion(date_as_int)
#' str(date_result)
#'
#' # and back again
#' int_result = rswat_date_conversion(date_result)
#' all.equal(date_as_int, int_result)
#'
#' # input can be data frame
#' date_as_df = data.frame(do.call(rbind, date_as_int))
#' all.equal(date_result, rswat_date_conversion(date_as_df))
#'
rswat_date_conversion = function(d, NA_zeros=TRUE, trim=TRUE) {

  # handle non-date inputs
  if( !is(d, 'Date') )
  {
    # matrix to data.frame
    if( nrow(d) == 0L ) return(d)
    if( is.matrix(d) ) d = as.data.frame(d)

    # check names and match to expected ones
    aliases = .rswat_gv_date_aliases()
    d = rswat_rename(d, aliases)
    d_nm = names(d)

    # deal with zeros as shorthand for first and last days of the year
    if(!NA_zeros)
    {
      # this finds the last Julian day of the end year
      last_date = rswat_date_conversion(data.frame(jday=1L, year=1L+d['end', 'year'])) - 1L
      if(d['start', 'jday'] == 0) d['start', 'jday'] = 1L
      if(d['end', 'jday'] == 0) d['end', 'jday'] = format(last_date, '%j')[[1L]]
    }

    # the year is mandatory for setting dates
    if( is.null(d[['year']]) ) stop('year not found')

    # set placeholder jday and month when missing
    if( is.null( d[['jday']] ) )
    {
      # this sets Jan 1 as date reported for yearly output
      if( is.null( d[['month']] ) ) { d[['jday']] = 1L } else {

        # assign the Julian date of the first of the month
        d[['jday']] = d[c('month', 'year')] |>
          apply(1L, \(x) paste0('1-', paste(x, collapse='-'))) |>
          as.Date(format='%j')
      }
    }

    # convert to Date via string
    d[['date']] = d[c('jday', 'year')] |>
      apply(1L, \(x) paste0(x, collapse='-')) |>
      as.Date(format='%j-%Y')

    # trim reduendant date fields on request
    if(trim) d_nm = d_nm[ !(d_nm %in% c(aliases, names(aliases)) ) ]
    return(d[, c('date', d_nm), drop=FALSE])
  }

  warning('this mode not implemented yet')
  # handle date inputs
  out_list = lapply(d, \(x) {

    c(jday = as.integer(as.character(x, format='%j')),
      year = as.integer(as.character(x, format='%Y')))
    }
  )

  return(out_list)
}


#' String distance ranking using `base::adist`
#'
#' Scores the elements in character vector `lu` according to their similarity with `pattern`.
#' Lower scores are better matches, with minimum 0 indicating a perfect match.
#'
#' All matching is case insensitive. When `lu_split=TRUE`, each entry of `lu` is split
#' at punctuation and white-space, and the function scores against each element separately.
#' The entire string is then assigned the score of the best matching element
#'
#' Scores returned by the function satisfy the following:
#'
#' 1. exact matches have score 0
#' 2. exact matches with sub-strings of `lu` have score in (0,1)
#' 3. approximate sub-string matches have score in [1, Inf)
#'
#' Results in group (2) are ordered according the relative difference in string length
#' between `pattern` and the respective element of `lu` (after splitting). Results in group
#' (3) are ordered according to the ratio of the number of string edits (Levenstein distance)
#' to the string length of `pattern`.
#'
#' The special search pattern '*' matches everything exactly (ie the function returns all 0s)
#'
#' Pipe characters in `pattern` are treated as logical OR delimiters: `pattern` gets split
#' at the pipes, string distance is computed separately for each component, then the parallel
#' minimum is returned. `pattern` is also split at any white-space characters, and the component
#' distances are averaged in the result (to get an approximation to OR). Pipes are parsed first,
#' then white-space.
#'
#' @param pattern character vector, the search keyword string
#' @param lu character vector, a set of strings to compare with `pattern`
#' @param lu_split logical, whether to split `lu` at punctuation and whitespace (see details)
#' @param split_c numeric, a small positive regularization constant (for internal use)
#' @param costs named list, costs for the three types of string edits, passed to `base::adist`
#'
#' @return logical, indicating if the directory has been assigned
#' @export
#'
#' @examples
#' # grab some text strings (table headers)
#' lu = .rswat_gv_cio_show()
#' lu
#'
#' # compute distances. zero indicates exact match
#' rswat_string_dist('n_var', lu)
#'
#' # sub-string is still the best match (but nonzero)
#' rswat_string_dist('var', lu)
#'
#' # repeat with punctuation splitting turned on to get near-exact match
#' rswat_string_dist('var', lu, lu_split=TRUE)
#'
#'
rswat_string_dist = function(pattern, lu,
                             lu_split = FALSE,
                             pattern_split = FALSE,
                             split_c = 1e-6,
                             costs = .rswat_gv_costs()) {

  # loop for 2+ patterns
  if( length(pattern) > 1 )
  {
    # bind results into a matrix
    dist_mat = do.call(cbind, lapply(pattern, \(p) {

      rswat_string_dist(pattern = p,
                        lu = lu,
                        lu_split = lu_split,
                        pattern_split = pattern_split,
                        split_c = split_c,
                        costs = costs)
    }))

    # one column for each pattern searched
    colnames(dist_mat) = pattern
    rownames(dist_mat) = lu
    return(dist_mat)
  }

  # handle empty search pattern case
  if( pattern == '*' ) return( rep(0, length(lu)) )

  # split pattern if requested
  if( pattern_split )
  {
    # get the distance first without splitting
    dist_out = rswat_string_dist(pattern = pattern,
                                 lu = lu,
                                 pattern_split = FALSE,
                                 lu_split = lu_split,
                                 split_c = split_c,
                                 costs = costs)

    # split at punctuation characters, ignoring beginning/end of string
    pattern = strsplit(pattern, '[[:punct:]]+|\\s+')[[1]]

    # return no-split distances if there was no punctuation
    is_punct = length(pattern) > 1
    if(!is_punct) return(dist_out)


    # call in a loop over all string elements in pattern (returns matrix)
    adist_mat = rswat_string_dist(pattern, lu, lu_split,
                                  pattern_split = FALSE,
                                  split_c = split_c,
                                  costs = costs)

    # compute combined distance for all lu elements
    adist_best = apply(adist_mat, 1L, \(x) min(x) + split_c * mean(x[which.min(x)]))
    is_improved = adist_best < dist_out
    if( any(is_improved) ) dist_out[is_improved] = adist_best[is_improved]
    return(dist_out)
  }

  # count the number of characters in pattern
  pattern_len = nchar(pattern)

  # convert to lowercase
  pattern = tolower(pattern)
  lu = tolower(lu)

  # count reference string lengths and assign to NAs max length + 1
  lu_len = nchar(lu)
  lu_len[is.na(lu_len)] = max(lu_len, na.rm=TRUE) + 1L

  # and compute relative length difference
  lu_rlen = 1 - ( pmin(lu_len, pattern_len) / pmax(lu_len, pattern_len) )

  # identify exact matches and initialize distances
  is_exact = lu %in% pattern
  dist_out = as.integer(!is_exact)
  if(pattern_len == 0L) return(dist_out)

  # find Levenstein distances to sub-strings as proportion of pattern length
  dist_sub = as.vector( adist(pattern, lu, costs=costs, partial=TRUE) ) / pattern_len
  dist_sub[ is.na(dist_sub) ] = max(dist_sub, na.rm=TRUE) + 1

  # use relative string length as base ranking for exact sub-string matches
  is_sub_exact = ( dist_sub == 0 ) & !is_exact
  if( any(is_sub_exact) ) dist_out[is_sub_exact] = lu_rlen[is_sub_exact]

  # rank approximate sub-string matches by Levenstein distance and relative lengths
  is_inexact = dist_sub > 0
  if( any(is_inexact) )
  {
    dist_sub_adj = dist_sub[is_inexact] + lu_rlen[is_inexact]
    dist_out[is_inexact] = 1 + ( dist_sub_adj / max(dist_sub_adj) )
  }

  # recompute for split look-up strings, if requested
  if( lu_split )
  {
    # split at punctuation characters, ignoring beginning/end of string
    lu_list = strsplit(lu, '[[:punct:]]+|\\s+')
    lu_len = sapply(lu_list, length)
    is_punct = lu_len > 1

    # recursive call to compute individual distances
    if( any(is_punct) )
    {
      # call in a loop over all string elements in lu that can be split
      adist_list = lapply(lu_list[is_punct], \(x) rswat_string_dist(pattern = pattern,
                                                                    lu = x,
                                                                    lu_split=FALSE,
                                                                    pattern_split = FALSE,
                                                                    split_c = split_c,
                                                                    costs = costs))

      # find best scores
      adist_best = sapply(adist_list, min)

      # apply a small penalty for splitting, based on median component scores
      id_other = !is_punct & (dist_out > 0)
      split_penalty = min(split_c, min( ifelse(any(id_other), dist_out[id_other], split_c)))
      adist_best = adist_best + split_penalty * sapply(adist_list, mean)

      # find minimum distance by list element and overwrite existing value
      is_improved = adist_best < dist_out[is_punct]
      if( any(is_improved) ) dist_out[is_punct][is_improved] = adist_best[is_improved]
    }
  }

  return(dist_out)
}


#' Approximate assignment based on a distance matrix
#'
#' Finds an assignment of `nrow(m)` labels to `ncol(m)` objects based on
#' the pairwise distances in `m` (where `m[i,j]` is the distance between the
#' `i`th label and the `j`th object), with no label assigned more than once.
#'
#' This uses a simple greedy algorithm where in each iteration the least distance
#' map is assigned and the label removed from the pool of candidates. The function
#' returns a vector indexing the label for each object. If there are fewer labels
#' than objects, this vector will have `NA`s for unmapped objects.
#'
#' @param m a non-negative numeric distance matrix
#'
#' @return a length-`ncol(m)` integer vector mapping labels or `NA`
#' @export
rswat_amatch = function(m) {

  # build a matrix of vectorization indices
  n_label = nrow(m)
  m_i = matrix(seq_along(m), n_label)

  # initialize to the least distance mapping in full matrix
  k = which.min(m)
  ij = matrix(nrow=0, ncol=2)
  colnames(ij) = c('i', 'j')

  #  greedy assignment until there's nothing left
  n_leftover = max(0, -diff(dim(m)))
  while( nrow(ij) < ( n_label - n_leftover ) ) {

    # index in the full matrix
    k_full = ifelse(nrow(ij) == 0, k, m_i[ -ij[,'i'], -ij[,'j'], drop=FALSE][k])

    # inverse vectorization locates row/column in full matrix
    j_new = as.integer( ceiling(k_full / n_label) )
    i_new = as.integer(k_full - n_label * (j_new-1))
    ij_new = c(i=i_new, j=j_new)

    # add to running list of rows/columns to exclude
    ij = rbind(ij, ij_new)
    ij = ij[order(ij[,'j']), , drop=FALSE]

    # find the least distance element in the updated sub-matrix
    k = which.min( m[ -ij[,'i'], -ij[,'j'], drop=FALSE] )
  }

  # return as a single vector
  out_vec = rep(NA, ncol(m))
  out_vec[ ij[,'j'] ] = ij[,'i']
  names(out_vec) = colnames(m)
  return(out_vec)
}



#' Create zip archives
#'
#' This code is copied from utils::zip and simplified to use default arguments,
#' but with the new option to omit the console output (with `quiet=TRUE`)
#'
#' @param zipfile character, the path to the zip file to write
#' @param files character vector, file paths to zip
#' @param quiet logical, if TRUE nothing is printed to the console
#'
#' @return status value (see utils::zip)
#' @export
rswat_zip = function(zipfile, files, quiet=FALSE) {

  zip = Sys.getenv('R_ZIPCMD', 'zip')
  flags = '-r9X'
  args = c(flags, shQuote(path.expand(zipfile)), shQuote(files))
  if(sum(nchar(c(args, Sys.getenv()))) + length(args) > 8000) {

    args = c(flags, "-@", shQuote(path.expand(zipfile)))
    input = files

  } else { input = NULL }

  if (.Platform$OS.type == 'windows') {

    invisible(system2(zip, args, input=input, invisible=TRUE, stdout=ifelse(quiet, FALSE, '')))

  } else {

    invisible(system2(zip, args, input=input, stdout=ifelse(quiet, FALSE, '')))
  }
}
