
#' Search variable names and description text in the SWAT+ inputs PDF
#'
#' Returns a tibble with SWA+ name/definition pairs matching `pattern`.
#'
#' The function uses `rswat_fuzzy_search` to look for parameter names containing
#' the string `pattern`. If `pattern` exactly matches a SWAT+ file name, the function
#' returns all known name/definition pairs for that file.
#
#' If `defs=TRUE` the function searches in definitions instead of names and file
#' names are ignored. Increase `fuzzy` to get approximate results or set it to -1
#' to get exact matches only (see `?rswat_fuzzy_search`).
#'
#' @param pattern character vector, the search keyword string
#' @param fuzzy integer controlling the number of results returned (see `?rswat_fuzzy_search`)
#' @param defs logical indicating to search definitions text instead of parameter names
#' @param .db rswat_db object for internal use
#'
#' @return tibble containing search results and their location in the PDF
#' @export
rswat_docs = function(pattern = '*',
                      fuzzy = 0L,
                      defs = FALSE,
                      .db = .rswat_db) {

  # check for file name in first argument only when searching names
  is_file_match = FALSE
  if(!defs)
  {
    # look for exact matches with file names
    is_file_match = .db[['docs']][['defs']][['file']] == pattern
    if( any(is_file_match) )
    {
      # pull all matching entries in order
      result = .db[['docs']][['defs']][ which(is_file_match), ] |>
        dplyr::select(-'desc_long') |>
        dplyr::mutate('distance' = -1)

      # print the file name (if any)
      if( nrow(result) > 0 )
      {
        file_name = head(unique(result[['file']]), 1)
        message( paste(nrow(result), 'definition(s) found for', file_name) )
      }
    }
  }

  # search docs
  if( !any(is_file_match) )
  {
    # perform the search
    result = rswat_fuzzy_search(pattern = pattern,
                                name_df = .db[['docs']][['defs']],
                                name = ifelse(defs, 'desc_long', 'name'),
                                fuzzy = fuzzy) |>
      dplyr::select(-'desc_long')
  }

  # skip sorting and files message in no-results case
  if( nrow(result) > 0 )
  {
    # skip this message for results coming from a file name look-up
    if( !any(is_file_match) )
    {
      # message about file names matched
      file_name = unique(result[['file']])
      search_type = ifelse(defs, 'definition(s)', 'parameter(s)')
      msg_result = paste(nrow(result), search_type, 'in', length(file_name), 'file(s)')

      # shows simpler message for default call with '*'
      if(pattern != '*') msg_result = paste0('"', pattern, '" matched to ', msg_result)
      message(msg_result)
    }

    # group search results by file via file-wise minimum distance
    scores_lu = result |>
      dplyr::group_by(file) |>
      dplyr::summarize(f_score=min(distance)) |>
      dplyr::arrange(f_score)

    # new grouped order for output
    idx_out = scores_lu[['f_score']][ match(result[['file']], scores_lu[['file']]) ] |> order()
    result = result[idx_out, ]

  } else {

    # print suggestions in 0 results case
    msg_empty = 'no results. Try increasing fuzzy'
    if(!defs) msg_empty = paste(msg_empty, 'or setting defs=TRUE to search definitions text')
    message(msg_empty)
  }

  # tidy up columns
  result = result |> dplyr::select(c('page_num', 'line_num', 'file', 'name', 'desc')) |>
    dplyr::rename('page'='page_num', 'line'='line_num')

  # return as tibble
  return( dplyr::tibble(result) )
}

#' Match SWAT+ variable names found in config files to definitions in documentation
#'
#' @param file_df input config file data
#' @param quiet logical, supresses console output
#' @param .db rswat_db object, for internal use
#'
#' @return a tibble with columns alias, match, definition, appended
#' @export
rswat_match_docs = function(file_df, quiet=FALSE, .db=.rswat_db) {

  # build file_df data frame from character argument
  if( is.character(file_df) ) file_df = rswat_find(file_df,
                                                   add_defs = FALSE,
                                                   quiet = TRUE,
                                                   .db = .db)
  # names to print
  nm_print = c(names(file_df), .rswat_gv_match_docs_trim(trim=3)) |> unique()

  # extract the file name(s) and return from empty case
  file_name = unique(file_df[['file']])
  if( is.null(file_name) ) return( file_df )

  # these columns get overwritten by the function
  n_file_df = nrow(file_df)
  file_df[['match_distance']] = rep(NA_real_, n_file_df)
  file_df[['alias']] = rep(NA_character_, n_file_df)
  file_df[['desc']] = rep(NA_character_, n_file_df)
  file_df[['match']] = rep(NA_character_, n_file_df)
  file_df[['id_alias']] = rep(NA_integer_, n_file_df)

  # multi-file case
  if( length(file_name) > 1 )
  {
    # make a list of results in a loop then combine into one data frame
    match_result_df = do.call(rbind, lapply(file_name, \(file_name_i) {

      # function call for a single file
      rswat_match_docs(file_df=file_name_i, quiet=TRUE, .db=.db)
    }))

    # merge the match data frame with the input
    nm_file = names(file_df)
    nm_extra = nm_file[ !( nm_file %in% names(match_result_df) ) ]
    file_df = merge(file_df[,c('file', 'name', nm_extra)], match_result_df)[, nm_print]

    # return selected columns in a tibble
    return(dplyr::tibble(file_df))
  }

  # single file case

  # message when no documentation found
  msg_fail = paste('no documentation found for file', file_name)

  # omit redundant rows
  nm_unique = unique( file_df[['name']] )
  file_df = file_df[match(file_df[['name']], nm_unique)[ seq_along(nm_unique) ],]

  # index of variable definitions for this file
  is_doc_relevant = .db[['docs']][['defs']][['file']] == file_name
  if( !any(is_doc_relevant) )
  {
    if(!quiet) message(msg_fail)
    return( file_df[, nm_print, drop=FALSE] )
  }

  # copy required elements from database
  docs_df = .db[['docs']][['defs']][is_doc_relevant,]

  # omit problematic strings from fuzzy name matching
  nm_omit = .rswat_gv_nm_nomatch()
  docs_df[['skip']] = docs_df[['name']] %in% nm_omit
  file_df[['skip']] = file_df[['name']] %in% nm_omit
  is_file_incl = !file_df[['skip']]
  is_docs_incl = !docs_df[['skip']]

  # end search if everything was skipped
  if( all(!is_file_incl) | all(!is_docs_incl) )
  {
    if( !quiet ) message(msg_fail)
    return( file_df[, nm_print, drop=FALSE] )
  }

  # find index of exact matches
  is_exact = file_df[['name']][is_file_incl] %in% docs_df[['name']][is_docs_incl]
  idx_exact_in_file = which(is_file_incl)[ is_exact ]
  idx_exact_in_docs = match(file_df[['name']][idx_exact_in_file], docs_df[['name']])
  any_exact = length(idx_exact_in_file) > 0
  if( any_exact )
  {
    # assign exact matches - these are fixed and not searchable below
    file_df[['alias']][ idx_exact_in_file ] = ''
    file_df[['match']][ idx_exact_in_file ] = '***'
    file_df[['desc']][ idx_exact_in_file ] = docs_df[['desc']][idx_exact_in_docs]
    file_df[['id_alias']][ idx_exact_in_file ] = idx_exact_in_docs
    file_df[['match_distance']][ idx_exact_in_file ] = rep(0, length(idx_exact_in_file))
    file_df[['skip']][ idx_exact_in_file ] = TRUE
    docs_df[['skip']][ idx_exact_in_docs ] = TRUE

    # update exclusion flags
    is_file_incl = !file_df[['skip']]
    is_docs_incl = !docs_df[['skip']]
  }

  # end of exact search
  if(  sum(is_file_incl) == 0 )
  {
    if( !quiet ) message('all variable names matched exactly')
    return( file_df[, nm_print, drop=FALSE] )
  }

  # initial fuzzy matching
  match_result = rswat_fuzzy_match(name_df = file_df, alias_df = docs_df)

  # check whether elements are in sequence
  is_unit_diff = diff(match_result[['id_alias']]) == 1L
  is_diagonal = match_result[['id_alias']] == seq(nrow(match_result))
  is_unit_diff[ is.na(is_unit_diff) ] = TRUE
  is_diagonal[ is.na(is_diagonal) ] = TRUE
  is_seq = is_diagonal | c(is_unit_diff, TRUE) | c(TRUE, is_unit_diff)

  #data.frame(match_result, is_seq=is_seq)

  # second round of matching for out-of-sequence elements
  if( !all(is_seq) )
  {
    # flag sequential elements (and their aliases) for exclusion from search
    match_result[['skip']][is_seq] = TRUE
    alias_exclude = match_result[['alias']][ match_result[['skip']] ]
    docs_df[['skip']][ docs_df[['name']] %in% alias_exclude ] = TRUE

    # second round of matching on non-sequential elements only
    match_result = rswat_fuzzy_match(name_df = match_result, alias_df = docs_df)
  }

  # check for items that were not mapped
  if( !quiet )
  {
    # known names without a match in docs
    is_missing = is.na(match_result[['alias']]) & !file_df[['skip']]
    msg_missing = paste(sum(is_missing), 'name(s) not matched:') |>
      paste( paste(match_result[['name']][is_missing], collapse=', ') ) |>
      rswat_truncate_txt()

    # variables in docs that couldn't be matched to the supplied names
    is_unused = !( docs_df[['name']] %in% match_result[['alias']] ) & !docs_df[['skip']]
    msg_unused = paste(sum(is_unused), 'unmatched definition(s) in documentation:') |>
      paste( paste(docs_df[['name']][is_unused], collapse=', ') ) |>
      rswat_truncate_txt()

    if( any(is_missing) ) message(msg_missing)
    if( any(is_unused) ) message(msg_unused)
  }

  # return match results
  #return(dplyr::rename(match_result[, nm_print], 'definition'='desc'))
  return(match_result[, nm_print])
}

# TODO: simplify and remove some of the arguments, rename, move to utils
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
  is_alias_used = if(skip) !alias_df[['skip']] else rep(TRUE, nrow(alias_df))

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
  name_df[['match_distance']][idx_name] = match_dist
  name_df[['alias']][idx_name] = alias_df[['name']][idx_alias]
  alias_extra = names(alias_df)[ !( names(alias_df) %in% c('name', 'skip') ) ]
  name_df[idx_name, alias_extra] = alias_df[idx_alias, alias_extra, drop=FALSE]

  # update star rankings then return
  name_df[['match']][idx_name] = ifelse(match_dist < 1, '**', '*')
  return(name_df)
}
