
#' Search SWAT+ config files for a keyword
#'
#' Returns a data frame of information on matches of `pattern` against SWAT+ file and
#' variable names. The default `'*'` matches everything, resulting in a data frame
#' summarizing ALL fields in the specified file(s).
#'
#' `fuzzy = -1` is for exact matches only, `fuzzy = 0` includes results where
#' `pattern` appears as a sub-string, and  `fuzzy > 0` includes the next `fuzzy` closest
#' matches. See `?rswat_fuzzy_search`.
#'
#' By default the function searches all loaded files. Otherwise, the function searches
#' in the subset of files specified by `include`, loading missing files as needed.
#' Get a list of files and their load state with `rswat_open()`.
#'
#' @param pattern character vector, the string(s) to search for
#' @param f character vector, file (or file group) names to include in search
#' @param fuzzy numeric, specifying tolerance for approximate matches (see details)
#' @param trim integer between 0-3, higher means more simplification in printed result
#' @param n_max positive integer, the maximum number of search results to return
#' @param quiet logical, supresses console output
#' @param .db rswat_db object, for internal use
#'
#' @return A data frame of information about SWAT+ names matching the search pattern(s)
#' @export
rswat_find = function(pattern = '*',
                      f = .db$get_loaded_files(),
                      fuzzy = 2L,
                      trim = 3L,
                      n_max = 10L,
                      quiet = FALSE,
                      .db=.rswat_db)
{
  # round fuzzy to integer
  fuzzy = ceiling(fuzzy)

  # load any missing files
  is_loaded = .db$is_file_loaded(f)
  if( any(!is_loaded) ) rswat_open(f[!is_loaded], .db=.db)

  # omit certain attributes depending on trim level
  nm_return = .rswat_gv_find_trim(trim)

  # TODO: append descriptions column to this names list

  # locate headers for requested files in fields info data-frame
  is_included = .db$line_df[['header']] & ( .db$line_df[['file']] %in% f )

  # return nothing in case where file.cio is the only element in f
  if( all(f %in% 'file.cio') )
  {
    # check if any other files are available for searching
    f_available = .db$get_loaded_files()
    is_f_alone = all(f_available %in% f)

    # message about the state of file.cio
    message(paste('rswat knows about', .db$report_known_files(), 'but only file.cio is loaded'))

    # only file.cio is available
    if(!is_f_alone) { message('no variables to display in file.cio') } else {

      # guidance for users who haven't loaded any files yet
      if(is_f_alone) message('load a file with rswat_open to make it searchable with rswat_find')
    }

    return(invisible())
  }

  # handle no-pattern calls
  if(pattern == '*')
  {
    # find index for requested files in fields info data-frame
    search_result = .db$line_df[is_included, ]
    rownames(search_result) = NULL

    # TODO: append descriptions here

    if(!quiet)
    {
      n_files = length(unique(search_result[['file']]))
      message(paste(nrow(search_result), 'variables in', n_files, 'file(s)'))
      print(search_result[, nm_return], quote=FALSE, right=FALSE)
      return(invisible(search_result[, nm_return]))
    }

    return(search_result[, nm_return])
  }

  # run the search on the names attribute
  vname_included = .db$line_df[['name']][is_included]
  search_result = rswat_fuzzy_search(pattern,
                                     lu = vname_included,
                                     lu_split = FALSE,
                                     fuzzy = fuzzy)

  # take subset of data-frame corresponding to matches, append distance scores
  idx_match = which(is_included)[ search_result[['order']] ]
  search_result = cbind(data.frame(distance=search_result[['distance']]), .db$line_df[idx_match, ])
  rownames(search_result) = NULL

  # TODO: append descriptions here

  # skip printing on request or if there were no results
  if( quiet | ( nrow(search_result) == 0 ) ) { search_result = search_result[, nm_return] } else {

    # prints the results to console
    search_result = rswat_show_search(results = search_result,
                                      fuzzy = fuzzy,
                                      n_max = n_max,
                                      nm_print = nm_return,
                                      .db = .db)
  }

  # return the whole data frame invisibly
  return(invisible(search_result))
}

#' Print the results of an rswat search
#'
#' Helper function for `rswat_find` and `rswat_docs`. This prints search results to the
#' console, dividing the results into 1-3 categories (exact, sub-string, or approximate.)
#' depending on `fuzzy`. Categories are based on the match strength scores in column
#' 'distance' (lower is better, see `?rswat_fuzzy_search`).
#'
#' `results` should have at least one additional column containing character strings
#' describing match results. `nm_print` controls which columns are printed (normally 'name'
#' and 'file', and possibly 'description'), and `n_max` controls many rows are printed.
#'
#' The function invisibly returns all rows of `results[, nm_print]` (independent of `n_max`)
#'
#' @param results data frame with numeric `distance` and character `name`, `file` columns
#' @param fuzzy integer, controlling the number of results returned (see `?rswat_find`)
#' @param n_max positive integer, the maximum number of search results to print
#' @param nm_print character vector, a subset of `names(results)` to return
#' @param .db rswat_db object, for internal use
#'
#' @return Returns (invisibly) the subset of `results` specified by `nm_print`
#' @export
rswat_show_search = function(results, fuzzy=2L, n_max=10L, nm_print=names(results), .db=.rswat_db)
{
  # handle empty results data frame
  n_results = nrow(results)
  if(n_results == 0)
  {
    message('no results')
    return(invisible(results))
  }

  # classify results, counting matches of each type
  results[['is_exact']] = results[['distance']] == 0
  results[['is_exact_sub']] = !results[['is_exact']] & ( results[['distance']] < 1 )
  results[['is_approx']] = !results[['is_exact']] & !results[['is_exact_sub']]
  n_exact = sum(results[['is_exact']])
  n_sub = sum(results[['is_exact_sub']])
  n_approx = sum(results[['is_approx']])

  # round distance scores for printing
  results[['distance']] = round(results[['distance']], 2)

  # trim the output data frame and keep a copy of the trimmed part for checks below
  results_omit = tail(results, pmax(0, n_results - n_max))
  results = head(results, n_results - nrow(results_omit))

  # add padding to right-align console printout columns
  for(nm_p in nm_print)
  {
    # nchar magic handles numeric types automatically here
    pad_len = pmax(0, max(nchar(results[[nm_p]])) - nchar(results[[nm_p]]) )
    results[[nm_p]] = Map(\(nm, len) paste0(nm, paste(rep(' ', len), collapse='')),
                          nm=results[[nm_p]],
                          len=pad_len)
  }

  # exact matches are always shown first
  n_omit = sum(results_omit[['is_exact']])
  msg_exact = ifelse(n_exact > 0, n_exact, 'none')
  msg_omit = ifelse(n_omit > 0, paste('showing', n_exact-n_omit, 'of', n_exact), msg_exact)
  message( paste('exact:', msg_omit) )
  if( any(results[['is_exact']]) )
  {
    print(results[ results[['is_exact']], nm_print], quote=FALSE, right=FALSE)
    cat('\n')
  }

  # message about sub-string matches
  if(fuzzy > 0)
  {
    n_omit = sum(results_omit[['is_exact_sub']])
    msg_sub = ifelse(n_sub > 0, n_sub, 'none')
    msg_omit = ifelse(n_omit > 0, paste('showing', n_sub-n_omit, 'of', n_sub), msg_sub)
    message(paste('sub-string:', msg_omit))
    if( any(results[['is_exact_sub']]) )
    {
      print(results[ results[['is_exact_sub']], nm_print], quote=FALSE, right=FALSE)
      cat('\n')
    }
  }

  # message about approximate matches
  if(fuzzy >= 1)
  {
    n_omit = sum(results_omit[['is_approx']])
    msg_n = ifelse(n_approx == 1, '', n_approx)
    msg_approx = ifelse(n_approx > 0, paste('showing first', msg_n), 'none shown')
    msg_approx_trim = paste('showing', n_approx-n_omit, 'of first', n_approx)
    msg_omit = ifelse(n_omit > 0, msg_approx_trim, msg_approx)
    message( paste('approximate:', msg_omit))
    if( any(results[['is_approx']]) )
    {
      print(results[ results[['is_approx']], nm_print], quote=FALSE, right=FALSE)
      cat('\n')
    }
  }

  # report omitted rows
  if( n_omit > 0 )
  {
    #cat('\n')
    message(paste(n_omit, 'result(s) not shown. Increase n_max to see more'))
  }

  # return the trimmed data frame
  return(invisible(results[, nm_print]))
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
rswat_fuzzy_search = function(pattern, lu, fuzzy=5L, lu_split=FALSE)
{
  # collapse vector pattern to series of OR and pass it to string distance function
  pattern = paste(unique(pattern), collapse='|')
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
      add_approx = which(is_approx)[order(dist_result[is_approx])[seq(fuzzy)]]
      if( any(is_approx) ) idx_result = c(idx_result, add_approx)
    }
  }

  # count results
  dist_result = dist_result[idx_result]
  n_results = length(idx_result)
  if(n_results == 0)
  {
    message('no results found. Try increasing fuzzy or loading more files')
    return(invisible())
  }

  # extract all available info on matches, append distance scores
  return(data.frame(order=idx_result, distance=dist_result, name=lu[idx_result]))
}






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



rswat_copy_desc = function(.db=.rswat_db)
{

}


