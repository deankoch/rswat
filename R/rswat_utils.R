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


#' Render table data to the specified character width for printing
#'
#'
#'
#' @param txt character vector, or a list of them (eg a data frame)
#' @param max_len integer, the maximum width allowed for printing (`NULL` to set automatically)
#' @param NA_char character, string to print in place of `NA`s
#' @param more_char character, string to indicate that a field has been shortened
#'
#' @return character vector or data frame (the same length as `txt`)
#' @export
rswat_truncate_txt = function(txt, max_len=NULL, NA_char='~', more_char='...', just='left')
{
  # widths of annotation strings
  n_NA = nchar(NA_char)
  n_more = nchar(more_char)

  # handle data frames
  if( is.data.frame(txt) )
  {
    # set default max_len based on R option (only tested on base windows GUI)
    if( is.null(max_len) ) max_len = unlist(options('width'))

    # measure width of print method output without last column
    idx_last = length(txt)
    len_first = utils::capture.output(print(txt[-idx_last])) |> nchar() |> max()

    # replace NA with character in all but last column
    txt[-idx_last] = replace(txt[-idx_last], is.na(txt[-idx_last]), NA_char)

    # set the max width for the last column, truncate, return everything
    max_len_last = max_len - len_first - 2L
    if(max_len_last < NA_char) stop('max_len too low. Try removing some columns')
    txt[[idx_last]] = rswat_truncate_txt(txt[[idx_last]], max_len_last, NA_char, more_char)
    return(txt)
  }

  # clean up input text and set default max_len to longest string length
  txt = gsub('\\s+', ' ', replace(txt, is.na(txt), NA_char), perl=TRUE) |> trimws()
  if( is.null(max_len) ) max_len = nchar(txt) |> max()

  # make sure the truncation symbol is not too long
  max_len_short = max_len - n_more
  if( max_len_short < 0 ) stop('max_len cannot be less than the width of more_char')

  # truncate then overwrite last n_more characters of truncated strings with more_char
  txt_short = substr(txt, 1L, max_len) |> trimws()
  is_short = nchar(txt_short) < nchar(txt)
  txt_short[is_short] = txt[is_short] |>
    substr(1L, max_len_short) |>
    trimws() |>
    paste0(more_char)

  # pad to max_len using white-space
  n_pad = pmax(0, max_len - nchar(txt_short))
  txt_pad = sapply(n_pad, \(n) paste(rep(' ', n), collapse=''))
  if( just == 'left' ) return( paste0(txt_short, txt_pad) )
  return( paste0(txt_pad, txt_short) )
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
                             pattern_split = FALSE,
                             split_c = 1e-6,
                             costs = .rswat_gv_costs())
{
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

    # call in a loop over all string elements in pattern that can be split
    adist_list = lapply(pattern, \(p) rswat_string_dist(pattern = p,
                                                        lu = lu,
                                                        lu_split = lu_split,
                                                        pattern_split = FALSE,
                                                        split_c = split_c,
                                                        costs = costs))

    # find best scores and apply a small penalty for splitting
    adist_best = apply(do.call(cbind, adist_list), 1L, min) + split_c

    # find minimum distance by list element and overwrite existing value
    is_improved = adist_best < dist_out
    if( any(is_improved) ) dist_out[is_improved] = adist_best[is_improved]
    return(dist_out)
  }

  # # count the number of characters in pattern
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
rswat_amatch = function(m)
{
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

#' Match names to aliases, merging other fields
#'
#' This uses rswat_amatch to match SWAT+ names to aliases in the PDF, then
#' merges the two data frames. It uses a greedy algorithm for assignment that
#' favors match patterns that are (nearly) in sequence
#'
#' Matching is done against the 'name' columns of both inputs
#'
#' @param name_df  input config file data
#' @param alias_df docs data
#' @param skip omit from search
#' @param alias_desc s
#' @param name_split s
#' @param alias_split  s
#' @param div_penalty internal
#' @param k_select diagonals
#'
#' @return a
#' @export
rswat_fuzzy_match = function(name_df,
                             alias_df,
                             skip = TRUE,
                             alias_desc = TRUE,
                             name_split = TRUE,
                             alias_split = TRUE,
                             div_penalty = 1,
                             k_select = NULL)
{
  # TODO: toggle symmetric, add lu_split etc

  # flags to include an item in searching
  is_name_used = if(skip) !name_df[['skip']] else rep(TRUE, nrow(name_df))
  is_alias_used = if(skip) !alias_df[['skip']] else rep(TRUE, nrow(name_df))

  # TODO: basic checks
  if( !any(is_name_used) | !any(is_alias_used) ) return(name_df)

  # set up pattern vector and look-up strings
  name_key = stats::setNames(nm=name_df[['name']][is_name_used])
  alias_nm = alias_df[['name']][is_alias_used]
  if(alias_desc) { alias_lu = alias_nm } else {

    # this mode assigns equal almost equal weight to description text
    alias_lu = do.call(paste, list(alias_nm, alias_df[['desc']][is_alias_used]))
    names(alias_lu) = alias_nm
  }

  # distance matrix for forward search
  d_mat = rswat_string_dist(pattern = name_key,
                            lu = alias_lu,
                            lu_split = alias_split,
                            pattern_split = name_split)

  # search in the reverse direction and take transpose to get new distances
  d_mat_t = t(rswat_string_dist(pattern = alias_nm,
                                lu = name_key,
                                lu_split = name_split,
                                pattern_split = alias_split))

  # penalty for divergence from input order
  if( is.null(k_select) ) k_select = min(diff(dim(d_mat_t)), 0):max(diff(dim(d_mat_t)), 0)
  pen_mat = Reduce('|', lapply(k_select, \(k) col(d_mat_t) - row(d_mat_t) == k ))
  pen_mat = div_penalty * !pen_mat

  # element-wise average of the two matrices plus penalty
  d_mat_m = pen_mat + ( d_mat_t + d_mat ) / 2
  rownames(d_mat_m) = alias_nm
  colnames(d_mat_m) = name_key

  # approximate matching
  idx_amatch = rswat_amatch(d_mat_m)

  # extract distances for result and indexing vectors for merging name_df, alias_df
  match_dist = diag(d_mat_m[idx_amatch, names(idx_amatch), drop=FALSE])

  # key to merge datasets
  idx_alias = which(is_alias_used)[ idx_amatch ]
  idx_name = which(is_name_used)[ name_key %in% names(idx_amatch) ]

  # assign mapping, distance to name_df, then copy any extra fields from alias_df
  name_df[['id_alias']][idx_name] = idx_alias
  name_df[['distance']][idx_name] = match_dist
  name_df[['alias']][idx_name] = alias_df[['name']][idx_alias]
  alias_extra = names(alias_df)[ !( names(alias_df) %in% c('name', 'skip') ) ]
  name_df[idx_name, alias_extra] = alias_df[idx_alias, alias_extra, drop=FALSE]

  # update star rankings then return
  name_df[['match']][idx_name] = ifelse(match_dist < 1, '**', '*')
  return(name_df)
}


