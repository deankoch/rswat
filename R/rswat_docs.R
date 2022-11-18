


#' Search variable names and description text in the SWAT+ I/O PDF
#'
#' @param pattern character vector, the search keyword string
#' @param f character vector, the file names (sections of document) to include in search
#' @param fuzzy integer, controlling the number of results returned (see details)
#' @param intro logical, if TRUE, prints the introductory text for a file
#' @param descriptions logical, if TRUE, searches description text instead of variable names
#' @param desc_len integer, maximum number description text characters to return (per variable)
#' @param pdf_df temporary, fix this
#'
#' @return data frame containing search results and their location in the PDF
#' @export
rswat_docs = function(pattern=NULL,
                      f=NULL,
                      fuzzy=1,
                      intro=FALSE,
                      descriptions=FALSE,
                      desc_len=50L,
                      .db=.rswat_db)
{

  # .db$docs$defs |> filter(file==f)
  # cat(.db$docs$comment[[f]])


  nm_show = c('file', 'name', 'desc_short')
  lu_name = ifelse(descriptions, 'desc', 'name')

  # file info mode
  null_pattern = is.null(pattern)
  if(null_pattern)
  {
    # a call without arguments
    if( is.null(f) )
    {
      # name_all = unique(pdf_df[['defs']][['name']])
      #table(pdf_df[['defs']][[lu_name]]) |> sort()
      f_all = names(.db[['docs']][['comment']])
      n_def = nrow(.db[['docs']][['defs']])
      message(paste('found', n_def, 'variable, definitions', length(f_all), 'files'))
      return(invisible(f_all))
    }

    # print the comment if requested
    if(intro)
    {
      cat(.db[['docs']][['comment']][[f]])
      return(invisible(pdf_df[['comment']][[f]]))
    }

    idx_out = .db[['docs']][['defs']][['file']] %in% f

  } else {

    fuzzy_result = rswat_fuzzy_search(pattern, .db[['docs']][['defs']][[lu_name]], fuzzy)
    idx_out = fuzzy_result[['order']]
    dist_out = fuzzy_result[['distance']]
    nm_show = c('distance', nm_show)
  }

  # TODO: implement files filter
  # TODO: implement file name in patter
  # TODO: implement description in pattern

  # copy the requested subset and truncate descriptions
  out_df = .db[['docs']][['defs']][idx_out,]
  if(!null_pattern) out_df[['distance']] = dist_out
  out_df[['desc_short']] = rswat_truncate_txt(out_df[['desc']], desc_len)
  return(out_df[, nm_show])
}



rswat_match_docs = function(file_df,
                            desc_weight=1,
                            desc_len=25L,
                            div_penalty=1,
                            k_select=NULL,
                            quiet=FALSE,
                            .db=.rswat_db)
{
  # handle character input to file_df
  if( is.character(file_df) ) file_df = rswat_find(f=file_df, quiet=TRUE)

  # define output columns
  nm_print = c('id_alias', 'name', 'alias', 'match', 'distance', 'desc')
  file_df[['distance']] = NA_real_
  file_df[['alias']] = NA_character_
  file_df[['desc']] = NA_character_
  file_df[['match']] = NA_character_
  file_df[['id_alias']] = NA_integer_

  # extract the file name(s) and return from empty case
  f = unique(file_df[['file']])
  if( is.null(f) ) return( file_df[, nm_print, drop=FALSE] )
  if( length(f) > 1 ) stop('multi-file case not implemented yet')

  # message when no documentation found
  msg_fail = paste('no documentation found for file', f)

  # omit redundant rows
  nm_unique = unique( file_df[['name']] )
  file_df = file_df[match(file_df[['name']], nm_unique)[ seq_along(nm_unique) ],]

  # index the variable definitions for this file
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

  # omit two problematic strings from fuzzy name matching
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
  idx_exact_in_file = which(is_file_incl)[ file_df[['name']][is_file_incl] %in% docs_df[['name']][is_docs_incl] ]
  idx_exact_in_docs = match(file_df[['name']][idx_exact_in_file], docs_df[['name']])
  any_exact = length(idx_exact_in_file) > 0
  if( any_exact )
  {
    # assign exact matches - these are fixed and not searchable below
    file_df[['alias']][ idx_exact_in_file ] = ''
    file_df[['match']][ idx_exact_in_file ] = '***'
    file_df[['desc']][ idx_exact_in_file ] = docs_df[['desc']][idx_exact_in_docs]
    file_df[['id_alias']][ idx_exact_in_file ] = idx_exact_in_docs
    file_df[['distance']][ idx_exact_in_file ] = rep(0, length(idx_exact_in_file))
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
  is_seq = is_diagonal | c(is_unit_diff, FALSE) | c(FALSE, is_unit_diff)
  data.frame(id=match_result$id_alias, is_seq=is_seq)


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
                                     name_split = FALSE,
                                     alias_split = FALSE,
                                     k_select = k_select,
                                     div_penalty = div_penalty)
  }

  # round distances for printing
  match_result[['distance']] = round(match_result[['distance']], 2)

  # check for items that were not mapped
  if( !quiet )
  {
    # known names without a match in docs
    is_missing = is.na(match_result[['alias']]) & !file_df[['skip']]
    msg_missing = paste(match_result[['name']][is_missing], collapse=', ')
    if( any(is_missing) ) message(paste(sum(is_missing), 'names not matched:', msg_missing))

    # variables in docs that couldn't be matched to the supplied names
    is_unused = !( docs_df[['name']] %in% match_result[['alias']] ) & !docs_df[['skip']]
    if( any(is_unused) )
    {
      msg_unused = paste(docs_df[['name']][is_unused], collapse=', ')
      message(paste(sum(is_unused), 'unmatched definitions in documentation:', msg_unused))
    }
  }

  # return match results
  return(match_result[, nm_print])
}

