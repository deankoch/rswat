
#' Search SWAT+ config files for a keyword
#'
#' Searches among loaded SWAT+ files for parameter names matching the search
#' query `pattern`, returning a tibble of information on the matches. If
#' `pattern` is a file name (and `vals=FALSE`) the function returns results
#' on all parameters in the file, automatically loading the file as needed.
#'
#' The default (`fuzzy = 0`) settings return results where `pattern` appears as a
#' sub-string, `fuzzy = -1` specifies exact matches only, and  `fuzzy > 0` returns,
#' in addition, the next `fuzzy` closest matches. See `?rswat_fuzzy_search`.
#'
#' Definitions are automatically matched to parameters when `add_defs=TRUE` (the
#' default). In cases of discrepancies in spelling, `rswat` attempts to match names
#' automatically to aliases. A three-star match score is assigned, with one or
#' two stars indicating some doubt.
#'
#' If `vals=TRUE`, the function searches parameter values (as strings) instead of
#' names and allows file name queries. This can be useful for finding (character
#' class) parameters that link to other parameters by naming a file.
#'
#' The special character `'*'` matches all parameters in all (loaded) files,
#' returning everything in a single tibble. This will take a few seconds if you have
#' `add_defs=TRUE`.
#'
#' Get a list of files and their load state with `rswat_open()` or `rswat_files()`.
#'
#' @param pattern character vector, the string(s) to search for
#' @param fuzzy numeric, specifying tolerance for approximate matches (see details)
#' @param vals logical, specifies to search values (in files) instead of parameter names
#' @param add_defs logical, appends 'match', 'alias', and 'desc' fields to results
#' @param quiet logical, supresses console output
#' @param .db rswat_db object, for internal use
#'
#' @return A data frame of information about SWAT+ names matching the search pattern(s)
#' @export
rswat_find = function(pattern = NULL,
                      fuzzy = 0L,
                      vals = FALSE,
                      add_defs = TRUE,
                      quiet = FALSE,
                      .db = .rswat_db) {

  # scan for changes and make a table of all recognized files on disk
  .db$refresh_cio_df(quiet=quiet)
  files_ondisk = .db$get_cio_df() |> dplyr::filter(known==TRUE) |> dplyr::pull(file)

  # get list of loaded files
  files_loaded = .db$get_loaded_files()

  # handle empty first argument
  if( is.null(pattern) )
  {
    paste0(length(files_loaded), '/', .db$report_known_files()) |>
      paste('are loaded and searchable.') |> message()
    message(paste('call rswat_find("*") to display all'))
    return(invisible())
  }

  # file name matching skipped when searching values
  is_file_match = FALSE
  if( !vals )
  {
    # look for exact matches with file names
    is_file_match = files_ondisk == pattern
    if( any(is_file_match) )
    {
      # load file(s)
      file_name = head(files_ondisk[is_file_match], 1)
      is_loaded = file_name %in% files_loaded
      if(!is_loaded) .db$open_config_batch(file_name, refresh=FALSE, quiet=quiet)
      files_loaded = c(files_loaded, file_name) |> unique()

      # load file info
      result = rswat_fuzzy_search(pattern='*', name_df=file_name, quiet=TRUE)

      # print the file name
      if( nrow(result) > 0 )
      {
        result = result |> dplyr::mutate('distance' = 0)
        if( !quiet ) message( paste(nrow(result), 'parameters(s) found in', file_name) )

      } else {

        # handle invalid file.cio request in this mode
        msg_help = 'file.cio has no parameter names. To search its contents, set vals=TRUE'
        if( all(files_loaded %in% 'file.cio') ) stop(msg_help)

        # otherwise there is a bug producing 0 parameter info for the file
        msg_bug = paste('rswat has no parameter info for', file_name)
        stop(msg_bug, '. Try opening with rswat_open?')
      }
    }

    # handle invalid file.cio request in this mode
    if( all(files_loaded %in% 'file.cio') )
    {
      message('Only file.cio is loaded.')
      message('To search parameter names, load another file.')
      message('To search within file.cio, call again with vals=TRUE')
      return(invisible())
    }
  }

  # search among loaded files
  if( !any(is_file_match) )
  {
    # perform the search and trim irrelevant columns
    result = rswat_fuzzy_search(pattern = pattern,
                                name_df = files_loaded,
                                name = ifelse(vals, 'string', 'name'),
                                what = ifelse(vals, 'string', 'header'),
                                fuzzy = fuzzy,
                                .db = .db)
  }

  # in no-results case skip sorting, definition matching, and files message
  if( nrow(result) == 0 )
  {
    if(!quiet) message('No results. Try increasing fuzzy or searching values with vals=TRUE')

  } else {

    # message about results not coming from a file name look-up
    if( !any(is_file_match) )
    {
      # message about number of files searched and matched
      file_name = unique(result[['file']])
      msg_domain = paste0('(searched ', length(files_loaded), ')')
      if( !quiet ) paste(nrow(result), 'result(s) for') |>
        paste0(' "', pattern, '"') |>
        paste('in', length(file_name), 'file(s)', msg_domain) |>
        message()
    }

    # match to definitions
    if(add_defs & !vals)
    {
      if(!quiet) cat('looking up definitions...\n')
      result = rswat_match_docs(result, .db=.db)
    }

    # group search results by file via file-wise minimum distance
    scores_lu = result |>
      dplyr::group_by(file) |>
      dplyr::summarize(f_score=min(distance)) |>
      dplyr::arrange(f_score)

    # new grouped order for output
    idx_out = scores_lu[['f_score']][ match(result[['file']], scores_lu[['file']]) ] |> order()
    result = result[idx_out, ]
  }

  # select columns to return
  relevant_columns = c('file', 'group', 'table', 'class', 'name')
  if(add_defs) relevant_columns = c(relevant_columns, 'alias', 'match', 'desc')
  if(vals) relevant_columns = c(relevant_columns, 'line_num', 'string')
  relevant_columns = relevant_columns[ relevant_columns %in% names(result) ]
  return( dplyr::tibble( dplyr::select(result, relevant_columns) ) )
}


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


#' Fuzzy text search for strings
#'
#' This calls `rswat_string_dist` to score how closely `pattern` matches with the
#' strings in the `name` column of `name_df`. The function appends a score to `name_df`
#' in column 'distance' (lower is better) and an integer column 'fuzzy' with values
#' in `c(-1, 0, 1)`, indicating the type of match (exact, sub-string, approximate).
#'
#' Results are sorted from best to worst, with row names indicating the original order
#' of `name_df`.
#'
#' With `fuzzy=-1`, only exact matches are returned; With `fuzzy=0`, sub-string matches
#' are also returned; and `fuzzy>0` returns an additional `fuzzy` approximate results.
#'
#' Fuzzy matching is based on Levenstein distance and relative string lengths.
#' `lu_split` and `pattern_split` control whether strings are split at punctuation
#' (with a small penalty to the score). See `?rswat_string_dist` for more information
#' on the scoring function.
#'
#' For internal use: `name_df` can also be a character vector of file names to include
#'
#' @param pattern character vector, the keyword to search for
#' @param name_df data frame, with column 'name' containing strings to search
#' @param name character, the column in `name_df` to search
#' @param fuzzy integer, controlling the number of results returned (see details)
#' @param lu_split logical, splits names at punctuation (passed to `rswat_string_dist`)
#' @param pattern_split logical, splits patterns at punctuation (passed to `rswat_string_dist`)
#' @param quiet logical, supresses console output
#'
#' @return A copy of `name_df` sorted by new column 'distance'
#' @export
#'
#' @examples
#' # grab some text strings (table headers) and add another column
#' name_df = data.frame(name=.rswat_gv_cio_show())
#' name_df[['foo']] = paste0('bar_', seq(nrow(name_df)))
#'
#' # compute distances. zero indicates exact match, higher indicates approximate match
#' rswat_fuzzy_search('n_var', name_df)
#'
#' # search term can be a substring
#' rswat_fuzzy_search('var', name_df)
#' rswat_fuzzy_search('n', name_df)
#'
#' # increase fuzzy to get more results
#' rswat_fuzzy_search('n_var', name_df, fuzzy=2)
#'
#' # repeat with punctuation splitting turned on to more closely match the 'n' prefix
#' rswat_fuzzy_search('n', name_df, fuzzy=2, pattern_split=TRUE, lu_split=TRUE)
#'
#' # fuzzing can help with typos
#' rswat_fuzzy_search('n_bar', name_df, fuzzy=1)
#' rswat_fuzzy_search('nvar', name_df, fuzzy=1)
#' rswat_fuzzy_search('num_vars', name_df, fuzzy=1)
#'
#' # false positives can happen due to length comparison
#' rswat_fuzzy_search('number_of_variables', name_df, fuzzy=2)
#'
#'
rswat_fuzzy_search = function(pattern,
                              name_df,
                              name = 'name',
                              fuzzy = 0L,
                              lu_split = FALSE,
                              pattern_split = FALSE,
                              quiet = FALSE,
                              what = 'header',
                              .db = .rswat_db)
{
  # handle db reference index input to name_df
  if( is.character(name_df) )
  {
    # when what='header', this is logical indicating a variable name, else it is the string itself
    is_searched = .db$get_line_df(what=what, f=name_df, drop=TRUE)
    if(what=='string')
    {
      # ignore empty values
      is_searched = nchar(is_searched) > 0

      # ignore header lines
      is_searched = is_searched & !( .db$get_line_df(what='header', f=name_df, drop=TRUE) )
    }

    # fetch the relevant table entries
    name_df = .db$get_line_df(f=name_df)[is_searched,]
  }

  # TODO: optimization for -1, 0, cases?
  # get distances to name column and copy to data frame
  name_df[['distance']] = rswat_string_dist(pattern = pattern,
                                            lu = name_df[[name]],
                                            lu_split = lu_split,
                                            pattern_split = pattern_split)

  # categorize distances
  name_df[['fuzzy']] = rep(0L, nrow(name_df))
  name_df[['fuzzy']][ name_df[['distance']] == 0 ] = -1L
  name_df[['fuzzy']][ name_df[['distance']] >= 1 ] = 1L

  # sort, preserving order in row names
  n_results = nrow(name_df)
  rownames(name_df) = NULL
  name_df = name_df[order(name_df[['distance']]), ]

  # identify the rows to return and truncate as needed
  is_returned = name_df[['fuzzy']] <= min(1L, fuzzy)
  idx_fuzzy = which( name_df[['fuzzy']][is_returned] == 1L )
  if( length(idx_fuzzy) > abs(fuzzy) ) is_returned[ idx_fuzzy[-seq(fuzzy)] ] = FALSE
  #if( !any(is_returned) & !quiet ) message('No results. Try increasing fuzzy')
  if(quiet) return(invisible(name_df[is_returned, ,drop=FALSE]))
  return(name_df[is_returned, , drop=FALSE])
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

