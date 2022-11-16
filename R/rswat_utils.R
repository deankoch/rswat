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
                             costs = .rswat_gv_costs())
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
    dist_mat = sapply(pattern_pipe, \(p) rswat_string_dist(p, lu, lu_split, split_c, costs))
    return( apply(dist_mat, 1, min) )
  }

  # detect logical AND in pattern
  pattern_split = strsplit(pattern, '\\s+')[[1]]
  pattern_split = pattern_split[nchar(pattern_split) > 0]
  if( length(pattern_split) > 1 )
  {
    # match the split patterns separately and return means
    dist_mat = sapply(pattern_split, \(p) rswat_string_dist(p, lu, lu_split, split_c, costs))
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
      # call in a loop over all string elements in lu that can be split
      split_list = lapply(lu_list[is_punct], \(x) {

        rswat_string_dist(pattern, x, FALSE, split_c, costs)
      })

      # find best scores
      adist_best = sapply(split_list, min)

      # apply a small penalty for splitting, based on median component scores
      id_other = !is_punct & (dist_out > 0)
      split_penalty = min(split_c, min( ifelse(any(id_other), dist_out[id_other], split_c)))
      adist_best = adist_best + split_penalty * sapply(split_list, mean)

      # find minimum distance by list element and overwrite existing value
      dist_out[is_punct] = adist_best
    }
  }

  return(dist_out)
}


#' Fuzzy text search based on rswat_string_dist
#'
#' This calls `rswat_string_dist` (with default `costs`) to score how closely
#' `pattern` matches with elements of `lu`, then sorts `lu` (best matching first), and
#' returns a subset. To return more results, increase `fuzzy`.
#'
#' For exact matches only, set `fuzzy=-1`. To also get results where `pattern` matches
#' sub-strings of `lu` exactly, set `fuzzy=0`. To get all of the above, plus the first
#' `fuzzy` approximate matches, set `fuzzy` to a positive integer.
#'
#' Fuzzy matching is based on Levenstein distance and relative string lengths. See
#' `?rswat_string_dist` for documentation on the scoring function and `lu_split`.
#'
#' The function returns results in a data frame, with 'order' indexing the input
#' `lu` sorted by least distance, 'distance' indicating the match strength (lower is
#' better), and 'name' indicating the string that was matched in `lu`.
#'
#' `lu` can also list of equal-length character vectors, in which case the function
#' is called on each vector separately with `fuzzy=Inf`
#'
#' @param pattern character vector, the keyword to search for
#' @param lu character vector, the strings to match against
#' @param fuzzy integer, controlling the number of results returned (see details)
#' @param lu_split logical, whether to split `lu` (see `?rswat_string_dist`)
#'
#' @return data frame with columns 'order', 'distance', and 'name'
#' @export
#'
#' @examples
#' # grab some text strings (table headers)
#' lu = .rswat_gv_cio_show()
#' lu
#'
#' # compute distances. zero indicates exact match, higher indicates approximate match
#' rswat_fuzzy_search('n_var', lu)
#'
#' # search term can be a substring
#' rswat_fuzzy_search('var', lu)
#'
#' # some ties resolved by ordering according to string length similarity
#' rswat_fuzzy_search('n', lu)
#'
#' # repeat with punctuation splitting turned on to more closely match the 'n' prefix
#' rswat_fuzzy_search(pattern='n', lu, lu_split=TRUE)
#'
#' # fuzzing can help with typos
#' rswat_fuzzy_search('n_bar', lu)
#' rswat_fuzzy_search('nvar', lu)
#'
#' # false positives can happen due to length comparison
#' rswat_fuzzy_search('num_variables', lu)
#'
#' # increase fuzzy to get more results
#' rswat_fuzzy_search('num_variables', lu, fuzzy=3)
#' rswat_fuzzy_search('num_variables', lu, fuzzy=5)
#'
rswat_fuzzy_search = function(pattern,
                              lu,
                              fuzzy = 5L,
                              lu_split = FALSE,
                              quiet = FALSE)
{
  # multiple pattern results returned in a list
  if( length(pattern) > 1 )
  {
    return(

      # evaluate patterns in a loop
      lapply(pattern, \(p) rswat_fuzzy_search(p,
                                              lu = lu,
                                              fuzzy = fuzzy,
                                              lu_split = lu_split,
                                              quiet = TRUE))
    )
  }

  # list (or dataframe) lookup tables are handled differently
  if( is.list(lu) )
  {
    # convert NAs to empty character
    lu = replace(lu, is.na(lu), '')

    # check for invalid input
    msg_bad_len = 'unequal lengths in list elements of lu'
    msg_bad_class = 'some elements of lu were not of character class'
    if( any( diff(sapply(lu, length)) != 0 ) ) stop(msg_bad_len)
    if( !all( sapply(lu, is.character) ) ) stop(msg_bad_class)
    if( length(lu) == 1L ) stop('lu list must have more than one element')

    # recursive call over columns
    results_list =  lapply(lu, \(lu_field) rswat_fuzzy_search(pattern,
                                                              lu = lu_field,
                                                              fuzzy = length(lu_field),
                                                              lu_split = lu_split,
                                                              quiet = TRUE))

    # keep only the first name in lu
    results = results_list[[1L]][ c('order', 'name', 'distance') ]

    # make a data frame from the other distances
    nm_append = paste0('distance_', seq_along(results_list)[-1])
    results_extra = data.frame( lapply(setNames(results_list[-1], nm=nm_append), \(r) {

      # reorder to match the order for the first name in lu
      r[['distance']][ match(results[['order']], r[['order']]) ]

    }) )

    # return the first fuzzy combined results as single data frame
    return( head(cbind(results, results_extra), fuzzy) )
  }

  # find string distances for single string pattern and character vector lu
  dist_result = rswat_string_dist(pattern, lu, lu_split)

  # initialize results vector to exact matches only
  is_exact = dist_result == 0
  idx_result = NULL
  if( any(is_exact) ) idx_result = which(is_exact)

  # add sub-string matches on request
  if( !(fuzzy < 0) )
  {
    # exclude exact matches and sort results, then add to results stack
    is_sub = !is_exact & (dist_result < 1)
    if( any(is_sub) ) idx_result = c(idx_result, which(is_sub)[ order( dist_result[is_sub] ) ])

    # approximate search mode
    if( fuzzy > 0 )
    {
      # append approximate results from 1 to `fuzzy`
      is_approx = !is_exact & !is_sub
      if( any(is_approx) )
      {
        # truncate if there aren't enough results
        fuzzy = min(fuzzy, sum(is_approx))
        add_approx = which(is_approx)[order(dist_result[is_approx])[seq(fuzzy)]]
        idx_result = c(idx_result, add_approx)
      }
    }
  }

  # count results
  dist_result = dist_result[idx_result]
  n_results = length(idx_result)
  if(n_results == 0)
  {
    if(!quiet) message('no results found. Try increasing fuzzy or loading more files')
    return(invisible(data.frame(order=integer(0), distance=numeric(0), name=character(0))))
  }

  # extract all available info on matches, append distance scores
  return(data.frame(order=idx_result, distance=dist_result, name=lu[idx_result]))
}


rswat_truncate_txt = function(txt, max_len=NULL, NA_char='NA', pad_char=' ', more_char='...')
{
  # handle data frame input
  if( is.data.frame(txt) ) return( data.frame(

    # loop over input columns
    lapply(txt, \(txt_column) {

      rswat_truncate_txt(txt_column,
                         max_len = max_len,
                         NA_char = NA_char,
                         pad_char = pad_char,
                         more_char = more_char)
    })
  ))

  # lengths of input strings (default max_len is input max length)
  n_src = nchar(txt, keepNA=FALSE)
  if( is.null(max_len) ) max_len = max(n_src)

  # create a truncated version of character string and convert NAs to empty strings
  txt_short = substr(gsub('\\s{2,}', pad_char, txt, perl=TRUE), 1L, max_len)
  txt_short[ is.na(txt_short) ] = NA_char
  n_short = nchar(txt_short)

  # vector of padding to add
  txt_pad = sapply(max_len - n_short, \(n) paste(rep(pad_char, n), collapse=''))

  # suffix indicating when a string has been truncated
  empty_char = paste(rep(pad_char, nchar(more_char)), collapse='')
  txt_suffix = ifelse(n_src > n_short, more_char, empty_char)
  return( paste0(txt_short, txt_pad, txt_suffix) )
}

