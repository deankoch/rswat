
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
  .db$refresh_cio_df()
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




# TODO: get rid of this
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
  #results_print = rswat_truncate_txt(results[, nm_print])
  results_print = results[, nm_print]

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

  # # print data frame of matches
  # print(results_print[idx_show, , drop=FALSE],
  #       row.names = T,
  #       quote = FALSE,
  #       right = FALSE)

  # report omitted rows
  cat('\n')
  msg_omit = paste(n_omit, 'result(s) not shown. Increase', trim_reason, 'to see more')
  if( n_omit > 0 ) message(msg_omit)

  # return the trimmed data frame without truncation
  return(invisible(results[, nm_print, drop=FALSE]))
}



