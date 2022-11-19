
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
#' @param docs logical, appends 'match', 'alias', and 'desc' fields to results
#' @param trim integer between 1-5, higher means more simplification in printed result
#' @param n_max positive integer, the maximum number of search results to return
#' @param quiet logical, supresses console output
#' @param .db rswat_db object, for internal use
#'
#' @return A data frame of information about SWAT+ names matching the search pattern(s)
#' @export
rswat_find = function(pattern = '*',
                      f = NULL,
                      fuzzy = 0L,
                      docs = TRUE,
                      trim = 3L,
                      n_max = 10L,
                      quiet = FALSE,
                      .db = .rswat_db)
{
  # returned columns depend on the trim level
  if( !(trim %in% 1:5) ) {

    # show all variables
    nm_print = names(rswat_scan_txt())

  } else { nm_print = .rswat_gv_find_trim(trim) }

  # append the columns returned by rswat_match_docs
  if( docs ) nm_print = unique( c(nm_print, .rswat_gv_match_docs_trim(trim)) )

  # scan for changes and make a list of all files on disk
  .db$refresh_cio_df()
  files_ondisk = .db$get_cio_df() |>
    dplyr::filter(known==TRUE) |>
    dplyr::select(file, group, type)

  # determine which files to look at
  if( is.null(f) )
  {
    # compare pattern with known file names
    if( !all(pattern %in% files_ondisk[['file']]) )
    {
      # by default search in all loaded files
      f = .db$get_loaded_files()

    } else {

      # if pattern matches a file exactly, return results for the file
      f = pattern
      pattern = '*'
    }
  }

  # if file.cio is the only file listed in f, return nothing
  if( all(f %in% 'file.cio') )
  {
    # check if any other files are available for searching
    f_available = .db$get_loaded_files()
    is_f_alone = all(f_available %in% f)

    # only file.cio is available
    if(!is_f_alone) { message('no variables to display in file.cio') } else {

      # guidance for users who haven't loaded any files yet
      message('load a file with rswat_open to make it searchable with rswat_find')
      message(paste('rswat knows about', .db$report_known_files(), 'but only file.cio is loaded'))
    }

    return(invisible())
  }

  # check for missing files
  is_loaded = .db$is_file_loaded(f)
  if( any(!is_loaded) )
  {
    # make sure the file(s) can be found on disk
    is_found = f %in% files_ondisk[['file']]
    if( all(is_found) | ( length(f) != 1L ) )
    {
      # load the requested file (or halt if files not found)
      message(paste('loading:', paste(f[!is_loaded], collapse=', ')))
      rswat_open(f[!is_loaded], .db=.db)

    } else {

      # file search requires quiet=FALSE
      msg_missing = paste('file', f, 'not found.')
      if(quiet) stop( paste(msg_missing, 'Repeat with quiet=FALSE to get file name suggestions') )

      # enter file search mode (this handles unknown f in length-1 case only)
      nm_print = names(files_ondisk)
      message( paste(msg_missing, 'Searching for similar file names...') )

      # search for this string among known file, group, type names
      lu = apply(files_ondisk, 1, \(x) paste(x, collapse='_'))
      search_result = rswat_fuzzy_search(pattern = f,
                                         lu = lu,
                                         lu_split = TRUE,
                                         fuzzy = nrow(files_ondisk),
                                         quiet = FALSE)

      # merge existing info on on matches and return from empty case
      search_result = cbind(search_result['distance'], files_ondisk[ search_result[['order']], ])
      if( nrow(search_result) == 0L ) return(invisible(search_result))

      # when exact (or sub-string) calls get 0 results, return first few approximate results
      any_exact = any(search_result[['distance']] == 0)
      any_sub_exact = any(search_result[['distance']] < 1)
      if( !any_exact & ( fuzzy == -1L ) ) fuzzy = 0L
      if( !any_sub_exact & ( fuzzy == -0L ) ) fuzzy = 1L

      # print the results to console if requested and return all rows invisibly
      return( rswat_show_search(results = search_result,
                                fuzzy = fuzzy,
                                n_max = n_max,
                                nm_print = nm_print,
                                quiet = quiet,
                                .db = .db) )
    }
  }

  # locate headers for requested files in fields info data-frame
  is_searched = .db$get_line_df(what='header', f=f, drop=TRUE)

  # handle default pattern
  if( pattern == '*' )
  {
    # find index for requested files in fields info data-frame
    search_result = .db$get_line_df(what=nm_print, f=f)[is_searched,]
    rownames(search_result) = NULL

    # append documentation and print on request
    if( docs ) search_result = rswat_match_docs(search_result, trim=0L)
    if(!quiet)
    {
      n_files = length(unique(search_result[['file']]))
      msg_files = ifelse(length(unique(f)) == 1L, f, paste(n_files, 'file(s)'))
      message(paste(nrow(search_result), 'variables in', msg_files))

      # print the data frame
      search_result_print = search_result[, nm_print, drop=FALSE] |> rswat_truncate_txt()
      if( nrow(search_result_print) > 0 )
      {
        print(search_result_print, quote=FALSE, right=FALSE)
        cat('\n')
      }
    }

    return(invisible(search_result[, nm_print, drop=FALSE]))
  }

  # search variable names
  vname_searched = .db$get_line_df(what='name', f=f, drop=TRUE)[is_searched]
  search_result = rswat_fuzzy_search(pattern = pattern,
                                     lu = vname_searched,
                                     lu_split = FALSE,
                                     fuzzy = fuzzy, #n_max,
                                     quiet = quiet)

  # merge existing info on on matches
  search_result = merge(search_result, .db$get_line_df(f=f)[is_searched,])
  if( docs ) search_result = rswat_match_docs(search_result, trim=0)

  # print the results to console if requested and return all rows invisibly
  return( rswat_show_search(results = search_result,
                            fuzzy = fuzzy,
                            n_max = n_max,
                            nm_print = nm_print,
                            quiet = quiet,
                            .db = .db) )
}

#' Print the results of an rswat search
#'
#' Helper function for `rswat_find`. This prints search results to the
#' console, dividing the results into 1-3 categories (exact, sub-string, or approximate.)
#' depending on `fuzzy`. Categories are based on the match strength scores in column
#' 'distance' (lower is better, see `?rswat_fuzzy_search`).
#'
#' `results` must have an additional column named 'nm_lu', containing the character
#' strings that were matched. `nm_print` controls which columns are printed (normally 'name'
#' and 'file', and possibly 'description'), and `n_max` controls many rows are printed.
#'
#' If a name in `nm_print` is not found among the columns of `results`, the function
#' uses the `get_cio_df` method of `.db` to look it up. Get a list of valid names for
#' this feature with `names(.rswat_db$get_cio_df())`.
#'
#' All rows of `results[, nm_print]` are returned invisibly, independent of `n_max`.
#'
#' @param results data frame with numeric `distance` and character `name` columns
#' @param fuzzy integer, controlling the number of results returned (see `?rswat_find`)
#' @param n_max positive integer, the maximum number of search results to print
#' @param nm_print character vector, usually a subset of `names(results)` (see details)
#' @param .db rswat_db object, for internal use
#' @param quiet logical, supresses console output
#'
#' @return Returns (invisibly) the subset of `results` specified by `nm_print`
#' @export
rswat_show_search = function(results,
                             fuzzy = 2L,
                             n_max = 10L,
                             nm_print = names(results),
                             quiet = FALSE,
                             .db = .rswat_db)
{
  # handle empty results data frame
  n_results = nrow(results)
  if(n_results == 0)
  {
    if(!quiet) message('no results')
    return(invisible(results))
  }

  # check for missing columns
  is_missing = !( c('distance', nm_print) %in% names(results) )
  if( any(is_missing) ) stop(paste(paste(nm_print, collapse=', '), 'missing from results'))

  # sort by distance and reset row names
  #results = results[order(results[['distance']]), ]
  rownames(results) = NULL

  # return the subset of the data frame in quiet mode
  if(quiet) return(invisible(results[, nm_print, drop=FALSE]))

  # classify results
  results[['is_exact']] = results[['distance']] == 0
  results[['is_exact_sub']] = !results[['is_exact']] & ( results[['distance']] < 1 )
  results[['is_approx']] = !results[['is_exact']] & !results[['is_exact_sub']]
  n_exact = sum(results[['is_exact']], na.rm=TRUE)
  n_sub = sum(results[['is_exact_sub']], na.rm=TRUE)
  n_approx = sum(results[['is_approx']], na.rm=TRUE)

  # round distance scores for printing
  results[['distance']] = round(results[['distance']], 2L)

  # trim the output data frame and keep a copy of the trimmed part for checks below
  results_omit = tail(results, pmax(0, n_results - n_max - 1L))
  results = head(results, n_results - nrow(results_omit))
  trim_reason = 'n_max'

  # # add padding to right-align console printout columns (keep a copy unchanged)
  results_return = results
  nm_not_desc = nm_print[nm_print != 'desc']
  results[, nm_not_desc] = apply(results[, nm_not_desc], 2L, rswat_truncate_txt)
  results[, nm_print] = rswat_truncate_txt(results[, nm_print])

  # exact matches are always shown first
  n_omit = sum(results_omit[['is_exact']], na.rm=TRUE)
  msg_exact = ifelse(n_exact > 0, n_exact, 'none')
  msg_omit = ifelse(n_omit > 0, paste('showing', n_exact-n_omit, 'of', n_exact), msg_exact)
  message( paste('exact:', msg_omit) )
  if( any(results[['is_exact']], na.rm=TRUE) )
  {
    print(results[ which(results[['is_exact']]), nm_print, drop=FALSE],
          row.names = FALSE,
          quote = FALSE,
          right = FALSE)

    cat('\n')
  }

  # message about sub-string matches
  if(fuzzy >= 0)
  {
    n_omit = sum(results_omit[['is_exact_sub']], na.rm=TRUE)
    msg_sub = ifelse(n_sub > 0, n_sub, 'none')
    msg_omit = ifelse(n_omit > 0, paste('showing', n_sub-n_omit, 'of', n_sub), msg_sub)
    message(paste('sub-string:', msg_omit))
    if( any(results[['is_exact_sub']], na.rm=TRUE) )
    {
      print(results[ which(results[['is_exact_sub']]), nm_print, drop=FALSE],
            row.names = FALSE,
            quote = FALSE,
            right = FALSE)

      cat('\n')
    }
  }

  # message about approximate matches
  if(fuzzy >= 1)
  {
    n_omit = sum(results_omit[['is_approx']], na.rm=TRUE)
    n_approx_show = min(n_approx-n_omit, fuzzy)
    if( n_approx_show < (n_approx-n_omit) ) trim_reason = 'fuzzy'
    msg_n = ifelse(n_approx_show == 1, '', n_approx_show)
    msg_approx = ifelse(n_approx_show > 0, paste('showing first', msg_n), 'none shown')
    msg_approx_trim = paste('showing', n_approx_show, 'of first', n_approx)
    msg_omit = ifelse(n_omit > 0, msg_approx_trim, msg_approx)
    message( paste('fuzzy:', msg_omit))
    if( n_approx_show > 0 )
    {
      idx_show = which(results[['is_approx']])[ seq(n_approx_show) ]
      print(results[idx_show, nm_print, drop=FALSE],
            row.names = FALSE,
            quote = FALSE,
            right = FALSE)

      cat('\n')
    }
  }

  # report omitted rows
  if( n_omit > 0 ) message(paste(n_omit, 'result(s) not shown. Increase',
                                 trim_reason, 'to see more'))

  # return the trimmed data frame wihtout truncation
  return(invisible(results_return[, nm_print, drop=FALSE]))
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
                              pattern_split = FALSE,
                              quiet = FALSE,
                              .db = .rswat_db)
{
  # multiple pattern results returned in a list
  if( length(pattern) > 1 )
  {
    return( lapply(pattern, \(p) rswat_fuzzy_search(pattern = p,
                                                    lu = lu,
                                                    fuzzy = fuzzy,
                                                    lu_split = lu_split,
                                                    pattern_split = pattern_split,
                                                    quiet = TRUE)))
  }

  # find string distances for single string pattern and character vector lu
  dist_result = rswat_string_dist(pattern = pattern,
                                  lu = lu,
                                  lu_split = lu_split,
                                  pattern_split = pattern_split)

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
