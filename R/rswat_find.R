
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
                      f = NULL,
                      fuzzy = 0L,
                      trim = 3L,
                      n_max = 10L,
                      quiet = FALSE,
                      .db = .rswat_db)
{
  # scan for changes and make a list of all files on disk
  .db$refresh_cio_df()
  files_ondisk = .db$get_cio_df(what='file')

  # set default f
  if( is.null(f) )
  {
    # file name can be supplied as argument pattern
    if(all(pattern %in% files_ondisk))
    {
      f = pattern
      pattern = '*'

    } else { f = .db$get_loaded_files() }
  }

  # check for missing files
  is_loaded = .db$is_file_loaded(f)
  if( any(!is_loaded) )
  {
    # make sure the file(s) can be found on disk
    is_found = f %in% files_ondisk
    if( any(!is_found) & ( length(f) == 1 ) )
    {
      # in quiet mode, halt on missing file, otherwise report it in a message
      msg_missing = paste('file', f, 'not found.')
      if(quiet) stop( paste(msg_missing, 'Repeat with quiet=FALSE to get file name suggestions') )
      message( paste(msg_missing, 'Searching for similar file names...') )

      # set default fuzzy for this mode
      #if( is.null(fuzzy) ) fuzzy = 0L

      # when f is a single string, search for this string among file names found on disk
      search_result = rswat_fuzzy_search(pattern = f,
                                         lu = files_ondisk,
                                         lu_split = TRUE,
                                         fuzzy = fuzzy,
                                         quiet = quiet)

      # finished no results case
      if( nrow(search_result) == 0 ) return(invisible())

      # TODO: add group to this result

      # print the results to console
      search_result = setNames(search_result[c('name', 'distance')], c('file', 'distance'))
      search_result = rswat_show_search(results = search_result,
                                        fuzzy = fuzzy,
                                        n_max = n_max,
                                        nm_print = 'file',
                                        .db = .db)

      return(invisible(search_result))
    }

    # missing files for vector f case trigger an error here (otherwise loads)
    rswat_open(f[!is_loaded], .db=.db)

  }

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

    # only file.cio is available
    if(!is_f_alone) { message('no variables to display in file.cio') } else {

      # guidance for users who haven't loaded any files yet
      message('load a file with rswat_open to make it searchable with rswat_find')
      message(paste('rswat knows about', .db$report_known_files(), 'but only file.cio is loaded'))
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

    # report the number of variables
    if(!quiet)
    {
      n_files = length(unique(search_result[['file']]))
      msg_files = ifelse(length(unique(f)) == 1, f, paste(n_files, 'file(s)'))
      message(paste(nrow(search_result), 'variables in', msg_files))
      print(search_result[, nm_return, drop=FALSE], quote=FALSE, right=FALSE)
    }

    return(invisible(search_result[, nm_return]))
  }

  # set a higher default fuzzy for this mode
  #if( is.null(fuzzy) ) fuzzy = 2L

  # run the search on the names attribute
  vname_included = .db$line_df[['name']][is_included]
  search_result = rswat_fuzzy_search(pattern = pattern,
                                     lu = vname_included,
                                     lu_split = FALSE,
                                     fuzzy = fuzzy,
                                     quiet = quiet)

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
rswat_show_search = function(results,
                             fuzzy = 2L,
                             n_max = 10L,
                             nm_print = names(results),
                             .db = .rswat_db)
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
    print(results[results[['is_exact']], nm_print, drop=FALSE], quote=FALSE, right=FALSE)
    cat('\n')
  }

  # message about sub-string matches
  if(fuzzy >= 0)
  {
    n_omit = sum(results_omit[['is_exact_sub']])
    msg_sub = ifelse(n_sub > 0, n_sub, 'none')
    msg_omit = ifelse(n_omit > 0, paste('showing', n_sub-n_omit, 'of', n_sub), msg_sub)
    message(paste('sub-string:', msg_omit))
    if( any(results[['is_exact_sub']]) )
    {
      print(results[ results[['is_exact_sub']], nm_print, drop=FALSE], quote=FALSE, right=FALSE)
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
    message( paste('fuzzy:', msg_omit))
    if( any(results[['is_approx']]) )
    {
      print(results[ results[['is_approx']], nm_print, drop=FALSE], quote=FALSE, right=FALSE)
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
  return(invisible(results[, nm_print, drop=FALSE]))
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
rswat_fuzzy_search = function(pattern, lu,
                              fuzzy = 5L,
                              lu_split = FALSE,
                              pattern_split = FALSE,
                              quiet = FALSE)
{
  # collapse vector pattern to series of OR
  pattern = paste(unique(pattern), collapse='|')

  # split pattern at punctuation (or repackage as length-1 list)
  if(pattern_split) pattern = strsplit(pattern, '[[:punct:]]|\\s+')[[1]]

  # pass pattern list to string distance function and compute mean
  dist_result = if( length(pattern) == 1 ) rswat_string_dist(pattern, lu, lu_split) else {

    # find distances of components and average the results
    dist_result = rowMeans(sapply(pattern, \(p) rswat_string_dist(p, lu, lu_split)))
  }

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
