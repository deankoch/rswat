
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
                      what = 'header',
                      trim = 4L,
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
  if( docs ) nm_print = unique( c(nm_print, .rswat_gv_match_docs_trim(trim=3L)) )

  # append columns required for what='tabular' mode
  if( what != 'header') nm_print = unique( c('line_num', 'string', nm_print) )

  # scan for changes and make a list of all files on disk
  .db$refresh_cio_df()
  files_ondisk = .db$get_cio_df() |>
    dplyr::filter(known==TRUE) |>
    dplyr::select(file, group, type)

  # determine which files to look at
  if( is.null(f) )
  {
    # compare pattern with known file names
    if( !all(pattern %in% files_ondisk[['file']]) | ( what != 'header' ) )
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
  if( all(f %in% 'file.cio') & ( what == 'header' ) )
  {
    # info for users about what happened
    if(!quiet)
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
    }

    return( invisible(data.frame(name=character(0))) )
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
      files_ondisk[['file_lookup']] = apply(files_ondisk, 1, \(x) paste(x, collapse=' '))
      search_result = rswat_fuzzy_search(pattern = f,
                                         name_df = files_ondisk,
                                         name = 'file_lookup',
                                         pattern_split = FALSE,
                                         lu_split = TRUE,
                                         fuzzy = nrow(files_ondisk),
                                         quiet = quiet)

      # when exact (or sub-string) calls get 0 results, return first approximate result
      if( !any(search_result[['fuzzy']] == -1) & ( fuzzy == -1L ) ) fuzzy = 0L
      if( !any(search_result[['fuzzy']] <= 0) & ( fuzzy == 0L ) ) fuzzy = 1L

      # print the results to console if requested and return all rows invisibly
      return( rswat_show_search(results = search_result,
                                fuzzy = fuzzy,
                                n_max = n_max,
                                nm_print = nm_print,
                                quiet = quiet,
                                .db = .db) )
    }
  }

  # handle default pattern
  if( pattern == '*' )
  {
    # locate headers for requested files in fields info data-frame
    is_searched = .db$get_line_df(what=what, f=f, drop=TRUE)

    # find index for requested files in fields info data-frame
    search_result = .db$get_line_df(what=nm_print, f=f)[is_searched,]
    rownames(search_result) = NULL

    # append documentation and print on request
    if( docs ) search_result = rswat_match_docs(search_result, trim=3L)
    if(!quiet)
    {
      n_files = length(unique(search_result[['file']]))
      msg_files = paste(n_files, 'file(s)')
      if( length(unique(f)) == 1L )
      {
        # change the message and omit file name and group from printout
        msg_files = f
        nm_print = nm_print[ !( nm_print %in% c('file', 'group', 'type') ) ]
      }

      # print a message
      message(paste(nrow(search_result), 'variable(s) in', msg_files))

      # print the data frame
      search_result_print = search_result[, nm_print, drop=FALSE] |> rswat_truncate_txt()
      if( nrow(search_result_print) > 0 )
      {
        print(search_result_print, right=FALSE)
        cat('\n')
      }
    }

    return(invisible(search_result[, nm_print, drop=FALSE]))
  }

  # search
  search_result = rswat_fuzzy_search(pattern = pattern,
                                     name_df = f,
                                     name = ifelse(what=='header', 'name', 'string'),
                                     pattern_split = TRUE,
                                     lu_split = FALSE,
                                     fuzzy = fuzzy,
                                     what = what,
                                     quiet = quiet)


  # TODO: optimization here by removing unnecessary match requests?
  # sort by distance and merge existing info on on matches
  if( docs ) search_result = rswat_match_docs(search_result, trim=3L)
  search_result = search_result[order(search_result[['distance']]), ]
  row.names(search_result) = NULL

  # print the results to console if requested and return all rows invisibly
  return( rswat_show_search(results = search_result,
                            fuzzy = fuzzy,
                            n_max = n_max,
                            nm_print = nm_print,
                            quiet = quiet,
                            .db = .db) )
}


#' Fuzzy text search for strings
#'
#' This calls `rswat_string_dist` to score how closely `pattern` matches with the
#' strings in the `name` column of `name_df`. The function appends a score to `name_df`
#' in column 'distance' (lower is better) and an integer column 'fuzzy' with values
#' in `c(-1, 0, 1)`, indicating the type of match (exact, sub-string, approximate).
#'
#' Results are sorted from best to worst before returning, with row names indicating
#' the original order of `name_df`.
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
    is_searched = .db$get_line_df(what=what, f=name_df, drop=TRUE)
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
  if( !any(is_returned) & !quiet ) message('No results. Try increasing fuzzy')
  if(quiet) return(invisible(name_df[is_returned, ,drop=FALSE]))
  return(name_df[is_returned, , drop=FALSE])
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

  # count results
  n_exact = sum(results[['fuzzy']]==-1, na.rm=TRUE)
  n_sub = sum(results[['fuzzy']]==0, na.rm=TRUE)
  n_approx = sum(results[['fuzzy']]==1, na.rm=TRUE)

  # round distance scores for printing
  results[['distance']] = round(results[['distance']], 2L)

  # trim the output data frame and keep a copy of the trimmed part for checks below
  trim_reason = 'n_max'
  results_omit = tail(results, pmax(0, n_results - n_max))
  results = head(results, n_results - nrow(results_omit))

  # copy and add padding to right-align console printout columns
  results_print = rswat_truncate_txt(results[, nm_print])

  # these vectors added to below
  idx_show = NULL
  msg_all = NULL

  # exact matches are shown first
  n_omit = sum(results_omit[['fuzzy']]==-1, na.rm=TRUE)
  msg_exact = ifelse(n_exact > 0, n_exact, 'none')
  msg_omit = ifelse(n_omit > 0, paste('showing', n_exact-n_omit, 'of', n_exact), msg_exact)
  if( any(results[['fuzzy']] == -1, na.rm=TRUE) )
  {
    idx_show = c(idx_show, which(results[['fuzzy']] == -1))
    msg_all = c(msg_all, paste('exact:', msg_omit))
  }

  # sub-string matches shown second
  if(fuzzy >= 0)
  {
    n_omit = sum(results_omit[['fuzzy']] == 0, na.rm=TRUE)
    msg_sub = ifelse(n_sub > 0, n_sub, 'none')
    msg_omit = ifelse(n_omit > 0, paste('showing', n_sub-n_omit, 'of', n_sub), msg_sub)
    if( any(results[['fuzzy']] == 0, na.rm=TRUE) )
    {
      idx_show = c(idx_show, which(results[['fuzzy']] == 0))
      msg_all = c(msg_all, paste('sub-string:', msg_omit))
    }
  }

  # approximate matches last
  if(fuzzy >= 1)
  {
    n_omit = sum(results_omit[['fuzzy']] == 1, na.rm=TRUE)
    n_approx_show = min(n_approx-n_omit, fuzzy)
    if( n_approx_show < (n_approx-n_omit) ) trim_reason = 'fuzzy'
    msg_n = ifelse(n_approx_show == 1, '', n_approx_show)
    msg_approx = ifelse(n_approx_show > 0, paste('showing first', msg_n), 'none shown')
    msg_approx_trim = paste('showing', n_approx_show, 'of first', n_approx)
    msg_omit = ifelse(n_omit > 0, msg_approx_trim, msg_approx)
    if( n_approx_show > 0 )
    {
      idx_show = c(idx_show, which(results[['fuzzy']] == 1)[ seq(n_approx_show) ])
      msg_all = c(msg_all, paste('fuzzy:', msg_omit))
    }
  }

  # message about type and number of matches
  message( paste(msg_all, collapse=', ') )
  cat('\n')

  # print data frame of matches
  print(results_print[idx_show, , drop=FALSE],
        row.names = T,
        quote = FALSE,
        right = FALSE)

  # report omitted rows
  cat('\n')
  msg_omit = paste(n_omit, 'result(s) not shown. Increase', trim_reason, 'to see more')
  if( n_omit > 0 ) message(msg_omit)

  # return the trimmed data frame without truncation
  return(invisible(results[, nm_print, drop=FALSE]))
}



