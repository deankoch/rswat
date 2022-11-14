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


#' Convert between two representations of date
#'
#' If `d` is a Date vector, the function returns a data frame with the same number of rows,
#' and attributes: Julian date ('j' ) and year ('yr'). Otherwise the function assumes `d`
#' is a data frame (or matrix, or list) of this form and it does the inverse, returning a vector
#' of Dates. `d` can also be a single integer vector c(j, yr)
#'
#' @param d either a Date object or an integer vector or list of them (or a matrix or dataframe)
#'
#' @return Either a Date vector or a data frame of j, yr, values
#' @export
#'
#' @examples
#' # convert from integers to Dates
#' n_test = 10
#' date_as_int = lapply(seq(n_test), \(x) c(j=sample.int(365, 1), yr=1950 + sample.int(100, 1)))
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
rswat_date_conversion = function(d) {

  # handle non-date inputs
  if( !is(d, 'Date') )
  {
    # coerce vector, matrix and data frame to list
    if( is.matrix(d) ) d = as.data.frame(d)
    if( is.data.frame(d) ) d = apply(d, 1, identity, simplify=FALSE)
    if( !is.list(d) ) d = list(d)

    # return the dates in a list
    return( as.Date(sapply(d, \(x) paste0(x[1], '-', x[2])), format='%j-%Y') )
  }

  # handle date inputs
  out_list = lapply(d, \(x) {

    c(j = as.integer(as.character(x, format='%j')),
      yr = as.integer(as.character(x, format='%Y')))
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
                             split_c = 1e-6,
                             costs = list(ins=4, del=1, sub=2))
{
  # handle empty search pattern case
  if( pattern == '*' ) return( rep(0, length(lu)) )

  # count the number of characters in pattern and check for logical OR
  pattern_len = nchar(pattern)
  pattern_pipe = strsplit(pattern, '\\|')[[1]]
  pattern_pipe = pattern_pipe[nchar(pattern_pipe) > 0]
  if( length(pattern_pipe) > 1 )
  {
    # match the split patterns separately and return parallel minimum
    dist_mat = sapply(pattern_pipe, \(p) rswat_string_dist(p, lu, lu_split, costs))
    return( apply(dist_mat, 1, min) )
  }

  # detect logical AND in pattern
  pattern_split = strsplit(pattern, '\\s+')[[1]]
  pattern_split = pattern_split[nchar(pattern_split) > 0]
  if( length(pattern_split) > 1 )
  {
    # match the split patterns separately and return means
    dist_mat = sapply(pattern_split, \(p) rswat_string_dist(p, lu, lu_split, costs))
    return( rowMeans(dist_mat) )
  }

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
    lu_list = strsplit(lu, '[[:punct:]]|\\s+')
    lu_len = sapply(lu_list, length)
    is_punct = lu_len > 1

    # recursive call to compute individual distances
    if( any(is_punct) )
    {
      # call in a loop over all string elements in lu that can be split, find best scores
      split_list = lapply(lu_list[is_punct], \(x) rswat_string_dist(pattern, x, F, split_c, costs))
      adist_best = sapply(split_list, min)

      # apply a small penalty for splitting, based on median component scores
      split_penalty = min(split_constant, min( dist_out[ !is_punct & (dist_out > 0) ] ))
      adist_best = split_penalty * sapply(adist_split, median)

      # find minimum distance by list element and overwrite existing value
      dist_out[is_punct] = adist_best
    }
  }

  return(dist_out)
}
