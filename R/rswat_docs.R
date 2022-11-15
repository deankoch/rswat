


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
  out_df[['desc']] = gsub('\\s{2,}', ' ', out_df[['desc']], perl=TRUE)
  desc_short = substr(out_df[['desc']], 1L, desc_len)
  desc_pad = sapply(desc_len - nchar(desc_short), \(n) paste(rep(' ', n), collapse=''))
  out_df[['desc_short']] = paste0(desc_short, desc_pad, ifelse(nchar(desc_pad) > 0, '   ', '...'))
  #
  return(out_df[, nm_show])
}



rswat_match_docs = function(f, desc_len=50L, .db=.rswat_db)
{
  # TODO: add exception for weather-wgn.cli
  # TODO: check for matching number of attributes (see nutrients.cha)

  f_results = rswat_find(f, quiet=TRUE, .db=.db)
  if(is.null(f_results)) return(NULL)

  is_in_docs = .db[['docs']][['defs']][['file']] %in% f
  f_docs = .db[['docs']][['defs']][is_in_docs, ]
  if(nrow(f_docs) == 0) return(NULL)

  # mean distance among description words (first `desc_len` characters only)
  f_docs[['desc_short']] = substr(f_docs[['desc']], 1L, desc_len)

  # find string distance scores
  f_nm = stats::setNames(nm=f_results[['name']])
  # search_result = lapply(f_nm, \(s) rswat_fuzzy_search(s, f_docs[['name']],
  #                                                      fuzzy = 3,
  #                                                      lu_split = TRUE,
  #                                                      pattern_split = TRUE,
  #                                                      quiet = TRUE))


  search_result = lapply(f_nm, \(s) {

    # distance from variable name to string pattern
    name_result = rswat_fuzzy_search(s, f_docs[['name']],
                                     fuzzy = Inf,
                                     lu_split = TRUE,
                                     pattern_split = TRUE,
                                     quiet = TRUE)

    # distance to short descriptions (average over all words)
    desc_result = rswat_fuzzy_search(s, f_docs[['desc_short']],
                                     fuzzy = Inf,
                                     lu_split = TRUE,
                                     pattern_split = TRUE,
                                     quiet = TRUE)

    # description results are in a different order
    idx_reorder = match(desc_result[['order']], name_result[['order']])
    name_result[['distance_desc']] = desc_result[['distance']][ idx_reorder ]
    name_result[['desc']] = desc_result[['name']][ idx_reorder ]

    # make a new combined distance score
    name_result[['distance_name']] = name_result[['distance']]
    name_result[['distance']] = rowMeans( name_result[c('distance_name', 'distance_desc')] )

    # exact name matches remain 0 (best score)
    name_result[['distance']][ name_result[['distance_name']] == 0 ] = 0
    return(name_result)
  })

  # remove exact matches from results list (they are already mapped)
  is_exact = sapply(search_result, \(x) any(x[['distance']]==0))
  if( any(is_exact) ) search_result = search_result[-which(is_exact)]

  # also remove from hits on other keywords, and remove empty results
  search_result = lapply(search_result, \(x) subset(x, !(name %in% f_nm[is_exact])))
  is_empty = sapply(search_result, \(x) nrow(x) == 0)
  if(any(is_empty)) search_result = search_result[!is_empty]

  # vector with named NA entries to fill in the loop below
  f_nm[!is_exact] = NA
  dist_nm = f_nm
  dist_nm[] = 0

  # add mappings in a loop until there's nothing left to map
  while( length(search_result) > 0 )
  {
    # re-order the remaining assignments from best to worst
    nm_ordered = names(sort(sapply(search_result, \(x) x[['distance']][1])))
    idx_rm = match(nm_ordered[1], names(search_result))

    # copy the best mapping and its distance
    nm_match = search_result[[idx_rm]][['name']][1]
    f_nm[ names(search_result)[idx_rm] ] = nm_match
    dist_nm[ names(search_result)[idx_rm] ] = search_result[[idx_rm]][['distance']][1]

    # update search list
    search_result = search_result[-idx_rm]
    search_result = lapply(search_result, \(x) subset(x, name != nm_match))
    is_empty = sapply(search_result, \(x) nrow(x) == 0)

    # clear empty results data frames
    if(any(is_empty)) search_result = search_result[!is_empty]
  }

  # copy the distances and aliases to results
  f_results[['distance']] = dist_nm
  f_results[['alias']] = f_nm
  f_results[['alias']][is_exact] = NA

  # copy description text via the new mapping
  idx_match = match(f_nm, f_docs[['name']])
  desc_txt = f_docs[['desc']][idx_match]

  # create a truncated version of descriptions
  desc_short = substr(gsub('\\s{2,}', ' ', desc_txt, perl=TRUE), 1L, desc_len)
  desc_short[ is.na(desc_short) ] = ''
  desc_pad = sapply(desc_len - nchar(desc_short), \(n) paste(rep(' ', n), collapse=''))
  f_results[['desc_short']] = paste0(desc_short, desc_pad, ifelse(nchar(desc_pad) > 0, '   ', '...'))
  f_results[['distance']][ is.na(idx_match) ] = NA


  return(f_results)
}
