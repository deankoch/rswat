


#' Search variable names and description text in the SWAT+ I/O PDF
#'
#' @param pattern character vector, the search keyword string
#' @param f character vector, the file names (sections of document) to include in search
#' @param fuzzy integer, controlling the number of results returned (see details)
#' @param intro logical, if TRUE, prints the introductory text for a file
#' @param descriptions logical, if TRUE, searches description text instead of variable names
#' @param desc_len integer, maximum number description text characters to return (per variable)
#' @param .db rswat_db object, for internal use
#'
#' @return data frame containing search results and their location in the PDF
#' @export
rswat_docs = function(name=NULL, f=NULL, quiet=FALSE, .db=.rswat_db)
{
  # check for valid input
  if( length(f) > 1 ) stop('multiple file names in the same call not supported')
  if( length(name) > 1 ) stop('multiple variable names in the same call not supported')

  # handle unspecified variable name
  if( is.null(name) )
  {
    # when called without arguments, return all file names listed in docs
    if( is.null(f) )
    {
      # all available docs
      f_docs = unique(.db$docs$defs$file)
      f_in_dir = f_docs %in% .db$get_loaded_files()

      # print choices
      if(!quiet)
      {
        # print a message about which of these files are loaded
        msg_known = paste(f_docs[f_in_dir], collapse=', ')
        msg_unknown = paste(f_docs[!f_in_dir], collapse=', ')
        message('select a file to see its documentation...')
        all_loaded = length(msg_unknown) == 0
        message(ifelse(all_loaded, 'all files loaded:', 'loaded:'))
        cat(paste0(msg_known, '\n'))
        if(!all_loaded)
        {
          message('not loaded:')
          cat(paste0(msg_unknown, '\n'))
        }
      }

      return(invisible(f_docs))
    }

    # list all variable names available for this file in docs
    name_docs = .db$docs$defs[.db$docs$defs$file == f, 'name']
    if(!quiet) message(paste('select a variable name:', paste(name_docs, collapse=', ')))
    return(invisible(name_docs))
  }

  # handle unspecified file name
  if( is.null(f) )
  {
    # extract all file names associated with this variable name
    f = unique(.db$docs$defs$file[ .db$docs$defs$name == name ])

    # handle no results
    if( length(f) == 0 )
    {
      msg_try = paste0('Try calling rswat_find("', name, '") to find an alias for this variable.')
      if(!quiet) message(paste('no results.', msg_try))
      return(invisible())
    }

    # handle multiple file matches
    if( length(f) > 1 )
    {
      msg_select = paste(f, collapse=', ')
      if(!quiet) message(paste('variable', name, 'found in multiple files. Select one:', msg_select))
      return(invisible(f))
    }
  }

  # more validity checks for input
  is_name_match = .db[['docs']][['defs']][['name']] == name
  is_file_match = .db[['docs']][['defs']][['file']] == f
  if( !any(is_name_match | is_file_match) ) stop('no matches for name or f')
  if( !any(is_name_match) ) return( rswat_docs(name=NULL, f=f, quiet=quiet, .db=.rswat_db) )
  if( !any(is_file_match ) ) return( rswat_docs(name=name, f=NULL, quiet=quiet, .db=.rswat_db) )
  is_match = is_name_match & is_file_match
  if( !any(is_match) ) stop(paste('variable', name, 'not found in file', f))

    # extract the matching documentation
  docs_df = .db[['docs']][['defs']][is_name_match & is_file_match,]
  if( nrow(docs_df) != 1L ) return('failed to find unique match')

  # print the selected variable name definition
  if( !quiet )
  {
    msg_loc = paste0('page ', docs_df[['page_num']], ', line ', docs_df[['line_num']], 'of PDF:')
    msg_file = paste0('(file ', f, ')')
    message( paste('printing definition of', name, msg_file, 'on', msg_loc) )
    cat( docs_df[['desc_long']] )
  }

  return(invisible(docs_df[['desc_long']]))
}


#' Match SWAT+ variable names found in config files to definitions in documentation
#'
#' @param file_df input config file data
#' @param desc_weight switch this to logical
#' @param desc_len how many characters of description to print
#' @param div_penalty internal
#' @param k_select k-diagonals
#' @param trim print level
#' @param quiet logical, supresses console output
#' @param .db rswat_db object, for internal use
#'
#' @return a
#' @export
rswat_match_docs = function(file_df,
                            desc_weight = 1,
                            desc_len = 100L,
                            div_penalty = 1,
                            k_select = NULL,
                            trim = 3L,
                            quiet = FALSE,
                            .db = .rswat_db)
{
  # handle character input to file_df
  if( is.character(file_df) ) file_df = rswat_find(f = file_df,
                                                   docs = FALSE,
                                                   trim = trim,
                                                   quiet = TRUE,
                                                   .db = .db)

  # returned columns depend on the trim level
  if( !( trim %in% 1:5 ) ) trim = 5
  nm_print = unique(c(names(file_df), .rswat_gv_match_docs_trim(trim)))

  # these columns get overwritten by the function
  n_file_df = nrow(file_df)
  file_df[['match_distance']] = rep(NA_real_, n_file_df)
  file_df[['alias']] = rep(NA_character_, n_file_df)
  file_df[['desc']] = rep(NA_character_, n_file_df)
  file_df[['match']] = rep(NA_character_, n_file_df)
  file_df[['id_alias']] = rep(NA_integer_, n_file_df)

  # extract the file name(s) and return from empty case
  f = unique(file_df[['file']])
  if( is.null(f) ) return( file_df[, nm_print, drop=FALSE] )

  # multi-file case
  if( length(f) > 1 )
  {
    # make a list of results in a loop then combine into one data frame
    match_result_list = do.call(rbind, lapply(f, \(f_i) {

      # function call for a single file
      rswat_match_docs(file_df = f_i,
                       desc_weight = desc_weight,
                       desc_len = desc_len,
                       div_penalty = div_penalty,
                       k_select = k_select,
                       trim = trim,
                       quiet = TRUE,
                       .db = .db)
    }))

    # merge the match data frame with the input and return selected columns
    nm_file = names(file_df)
    nm_extra = nm_file[ !( nm_file %in% names(match_result_list) ) ]
    return(merge(file_df[,c('file', 'name', nm_extra)], match_result_list)[, nm_print])
  }

  # message when no documentation found
  msg_fail = paste('no documentation found for file', f)

  # omit redundant rows
  nm_unique = unique( file_df[['name']] )
  file_df = file_df[match(file_df[['name']], nm_unique)[ seq_along(nm_unique) ],]

  # index of variable definitions for this file
  is_docs_on_f = .db[['docs']][['defs']][['file']] == f
  if( !any(is_docs_on_f) )
  {

    if( !quiet ) message(msg_fail)
    return( file_df[, nm_print, drop=FALSE] )
  }

  # copy required elements from database
  docs_df = data.frame(

    # name and truncated description
    name = .db[['docs']][['defs']][['name']][is_docs_on_f],
    desc = substr(.db[['docs']][['defs']][['desc']][is_docs_on_f], 1L, desc_len)
  )

  # omit problematic strings from fuzzy name matching
  nm_omit = .rswat_gv_nm_nomatch()
  docs_df[['skip']] = docs_df[['name']] %in% nm_omit
  file_df[['skip']] = file_df[['name']] %in% nm_omit

  # end search if everything was skipped
  is_file_incl = !file_df[['skip']]
  is_docs_incl = !docs_df[['skip']]
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
  match_result = rswat_fuzzy_match(name_df = file_df,
                                   alias_df = docs_df,
                                   skip = TRUE,
                                   alias_desc = TRUE,
                                   name_split=TRUE,
                                   alias_split=TRUE,
                                   k_select = k_select,
                                   div_penalty = div_penalty)

  # check whether elements are in sequence
  is_unit_diff = diff(match_result[['id_alias']]) == 1L
  is_diagonal = match_result[['id_alias']] == seq(nrow(match_result))
  is_unit_diff[ is.na(is_unit_diff) ] = TRUE
  is_diagonal[ is.na(is_diagonal) ] = TRUE
  is_seq = is_diagonal | c(is_unit_diff, TRUE) | c(TRUE, is_unit_diff)

  data.frame(match_result, is_seq=is_seq)

  # second round of matching for out-of-sequence elements
  if( !all(is_seq) )
  {
    # flag sequential elements (and their aliases) for exclusion from search
    match_result[['skip']][is_seq] = TRUE
    alias_exclude = match_result[['alias']][ match_result[['skip']] ]
    docs_df[['skip']][ docs_df[['name']] %in% alias_exclude ] = TRUE

    # second round of matching on non-sequential elements only
    match_result = rswat_fuzzy_match(name_df = match_result,
                                     alias_df = docs_df,
                                     skip = TRUE,
                                     alias_desc = TRUE,
                                     name_split = TRUE,
                                     alias_split = TRUE,
                                     k_select = k_select,
                                     div_penalty = div_penalty)
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
  return(match_result[, nm_print])
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
  name_df[['match_distance']][idx_name] = match_dist
  name_df[['alias']][idx_name] = alias_df[['name']][idx_alias]
  alias_extra = names(alias_df)[ !( names(alias_df) %in% c('name', 'skip') ) ]
  name_df[idx_name, alias_extra] = alias_df[idx_alias, alias_extra, drop=FALSE]

  # update star rankings then return
  name_df[['match']][idx_name] = ifelse(match_dist < 1, '**', '*')
  return(name_df)
}
