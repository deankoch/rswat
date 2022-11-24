# helpers for rswat_db

#' Helper for show method of rswat_db
#'
#' `ok_char` should have names 'yes', 'fail', 'no', and 'sub', corresponding to
#'  items that succeed, fail, have yet to be attempted, and those that are sub-tasks .
#'
#' @param ok_char character vector of symbols to use for bullets
#' @param .db rswat_db object reference, for internal use
#'
#' @return nothing
#' @export
rswat_summarize_db = function(ok_char=.rswat_ok_char(), .db=.rswat_db)
{
  # matrix of categories and headers for printout
  show_mat = list(dir = c('directory:', 'not assigned'),
                  exe = c('simulator:', 'not assigned'),
                  time = c('time.sim:', 'not loaded'),
                  print = c('print.prt:', 'not loaded'),
                  climate = c('climate:', 'weather-sta.cli not loaded'),
                  cio = c('file.cio:', 'not loaded')) |> data.frame() |> t()

  # initialize headers and align to common width
  colnames(show_mat) = c('txt', 'alt')
  n_head = max(nchar(show_mat[, 'txt']))
  swat_msg = ok_char['no'] |>
    paste(rswat_truncate_txt(show_mat[, 'txt'], n_head, just='right')) |>
    paste(show_mat[,'alt']) |>
    stats::setNames( rownames(show_mat) )

  # build message about SWAT directory
  swat_dir = .db$get_swat_dir()
  swat_dir_msg = ok_char['yes'] |>
    paste( rswat_truncate_txt(show_mat['dir', 'txt'], n_head, just='right') ) |>
    paste(swat_dir)

  # build message about executable path
  exe_path = .db$get_exe_path()
  exe_path_msg = ok_char['yes'] |>
    paste( rswat_truncate_txt(show_mat['exe', 'txt'], n_head, just='right') ) |>
    paste(exe_path)

  # update messages about paths
  swat_msg['dir'] = ifelse(is.na(swat_dir), swat_msg['dir'], swat_dir_msg)
  swat_msg['exe'] = ifelse(is.na(exe_path), swat_msg['exe'], exe_path_msg)

  # check what's loaded
  f_check = c(cio='file.cio', time='time.sim', print='print.prt', climate='weather-sta.cli')
  is_loaded = .db$is_file_loaded(f_check, check_dir=FALSE)

  # print file info message
  if( is_loaded['file.cio'] )
  {
    # create the file.cio message
    swat_msg['cio'] = ok_char['yes'] |>
      paste( rswat_truncate_txt(show_mat['cio', 'txt'], n_head, just='right') ) |>
      paste('reports', .db$report_known_files(), '\n')

    # create two time period messages
    nm_check = c('time', 'print')
    for(nm in nm_check)
    {
      # check if file name is loaded
      f = f_check[nm]
      if( is_loaded[f] )
      {
        # validity check
        msg_time = .db$get_sim_dates(prt=(nm=='print'), render=TRUE)
        msg_invalid = paste0('invalid dates! Check them with rswat_open("', f, '")')
        if( anyNA(msg_time) )
        {
          # failed to parse dates - something wrong with the file
          swat_msg[nm] = ok_char['fail'] |>
            paste( rswat_truncate_txt(show_mat[nm, 'txt'], n_head, just='right') ) |>
            paste(msg_invalid)

        } else {

          # append a string indicating the time period to simulate
          swat_msg[nm] = ok_char['yes'] |>
            paste( rswat_truncate_txt(show_mat[nm, 'txt'], n_head, just='right') ) |>
            paste(msg_time)
        }
      }
    }

    # create weather-sta.cli message
    if( is_loaded['weather-sta.cli'] )
    {
      # this one possibly spans multiple lines
      swat_msg['climate'] = ok_char['yes'] |>
        paste( rswat_truncate_txt(show_mat['climate', 'txt'], n_head, just='right') ) |>
        paste(rswat_weather_report(lazy=FALSE, n_head=n_head, quiet=TRUE, .db=.db))

    }
  }

  # line break string to separate categories
  hbreak_msg = paste(c(rep(' ', 2L), rep('-', n_head)), collapse='')

  message('rswat')
  message(hbreak_msg)
  cat(paste0(swat_msg['dir'], '\n', swat_msg['exe'], '\n'))
  message(hbreak_msg)

  cat(swat_msg['climate'], '\n')
  message(hbreak_msg)
  cat(paste0(swat_msg['time'], '\n', swat_msg['print'], '\n'))
  message(hbreak_msg)

  cat(swat_msg['cio'], '\n')

  return(invisible())
}


#' Return a printable string describing state of climate files in the SWAT+ project
#'
#' @param lazy logical
#' @param n_head integer
#' @param quiet logical, if FALSE the function passes its output to `base::cat`
#' @param ok_char character vector of symbols to use for bullets
#' @param .db rswat_db object reference, for internal use
#'
#' @return returns the character string invisibly
#' @export
rswat_weather_report = function(lazy = TRUE,
                                n_head = 10L,
                                quiet = FALSE,
                                ok_char = .rswat_ok_char(),
                                .db = .rswat_db)
{
  # halt if the file is not available
  f_sta = 'weather-sta.cli'
  if( !.db$is_file_loaded(f_sta) )
  {
    # attempt to load then return from not loaded case
    if(lazy) rswat(include=f_sta, quiet=TRUE, .db=.db)
    if( !.db$is_file_loaded(f_sta) ) stop(paste(f_sta, 'not loaded'))
  }
  # prefix for sub-headings
  sub_nm = c('wgn', 'sta')
  msg_sub_prefix = paste(ok_char['sub'], paste0(sub_nm, ':')) |>
    rswat_truncate_txt(n_head + 2L, just='right') |>
    stats::setNames(sub_nm)

  # initialize three part message
  msg_weather = paste(nrow(.db$get_stor_df(f_sta)[[1L]]), 'stations in', f_sta)
  msg_simulated = paste(msg_sub_prefix['wgn'], 'simulating none')
  msg_observed = paste(msg_sub_prefix['sta'], 'none loaded')
  msg_no_load = paste(c(msg_weather, msg_simulated, msg_observed), collapse='\n')

  # collect the date ranges for each file group (-1 for failure, NA for simulated)
  dates_df = rswat_weather_dates(lazy=lazy, .db=.db)
  v_nm = names(dates_df)

  # check for weather generator prompts and groups not yet loaded
  is_loaded = !sapply(dates_df, is.integer)
  is_simulated = sapply(dates_df, anyNA)

  # update the simulation message
  if( any(is_simulated) )
  {
    msg_sim = paste(v_nm[is_simulated], collapse=', ')
    msg_simulated = paste(msg_sub_prefix['wgn'], 'simulating', msg_sim)
  }

  # update the stations message
  is_loaded_station = !is_simulated & is_loaded
  if( !any(is_loaded_station) )
  {
    # update message with variable names
    msg_observed = msg_sub_prefix['sta'] |>
      paste0(' ', paste(v_nm[!is_simulated], collapse=', ')) |>
      paste0(ifelse(any(!is_simulated), ' (none loaded)', 'none'))

  } else {

    # build message about full date range
    dates_range = dates_df[, is_loaded_station] |> as.matrix() |> range()
    msg_period = paste0('[', paste(dates_range, collapse=' to '), ']')

    # identify variables spanning full date range
    is_full = apply(dates_df[, is_loaded_station] == dates_range, 2L, all)

    # message about variables spanning subset of full range
    msg_v_partial = paste(v_nm[is_loaded_station][!is_full], collapse=', ')

    # message about these variables
    msg_v_full = paste(v_nm[is_loaded_station][is_full], collapse=', ')
    msg_observed = msg_sub_prefix['sta'] |>
      paste(msg_period) |>
      paste('for', msg_v_full) |>
      paste(ifelse(any(!is_full), paste0('(', msg_v_partial, ')'), ''))

  }

  # optionally print then return character string invisibly
  msg_out = paste(c(msg_weather, msg_simulated, msg_observed), collapse='\n')
  if(!quiet) cat(msg_out)
  return(invisible(msg_out))

}



# fetch the set of dates contained in the weather input files
rswat_weather_dates = function(lazy=TRUE, .db=.rswat_db)
{
  f_sta = 'weather-sta.cli'
  if( !.db$is_file_loaded(f_sta) ) stop(paste(f_sta, 'not loaded'))

  # load and clean the table
  sta_files = .db$get_stor_df(f_sta)[[1L]]
  sta_files = sta_files[ !( names(sta_files) %in% c('name', 'wgn', 'wnd_dir', 'atmo_dep') ) ]
  is_simulated = sta_files == 'sim'

  # check if files along each column have been loaded
  is_loaded = sapply(sta_files, .db$is_file_loaded)
  is_checked = is_loaded | is_simulated
  is_group_loaded = apply(is_checked, 2L, all)

  # initialize outputs (-1 indicates failure)
  out_msg = head(sta_files, 1L)
  out_msg[1L,] = rep('not loaded', length(sta_files))
  out_dates = rep(list(integer(2)-1L), length(sta_files)) |> stats::setNames(names(out_msg))

  # deal with opened columns
  if( any(is_group_loaded) )
  {
    for(nm in names(out_dates)[is_group_loaded])
    {
      # identify empty weather files that will be covered by the weather generator
      f = sta_files[, nm, drop=FALSE]
      is_sim = f == 'sim'
      msg_count = paste0(sum(!is_sim), ' stations')

      if(all(is_sim))
      {
        out_msg[nm] = 'simulated'
        out_dates[[nm]] = rep(as.Date(NA), 2L)

      } else {

        # extract all start dates
        start_df = do.call(rbind, lapply(.db$stor_df[ f[!is_sim] ], \(x) {
          head(x[[2L]], 1L) |> rswat_date_conversion()
        }) )

        # extract all end dates
        end_df = do.call(rbind, lapply(.db$stor_df[ f[!is_sim] ], \(x) {
          tail(x[[2L]], 1L) |> rswat_date_conversion()
        }) )

        # validity check
        nm_out = c('start', 'end')
        msg_unequal = paste('unequal', nm_out, 'dates in weather files:', paste(f, collapse=', '))
        if( !all(diff(start_df[['date']]) == 0) ) warning(msg_unequal[1L])
        if( !all(diff(end_df[['date']]) == 0) ) warning(msg_unequal[2L])

        # make a group label for printing
        dates_range = c(start_df[['date']][1L], end_df[['date']][1L])
        step_years = round(difftime(dates_range[2L], dates_range[1L], units='days') / 365, 2)
        step_msg = paste0('(', step_years, ' years)')

        # copy results
        out_msg[nm] = paste(msg_count,
                            paste(paste0('[', dates_range, ']'), collapse=' to '), step_msg)
        out_dates[[nm]] = dates_range
      }
    }

    dates_df = data.frame(out_dates)
    row.names(dates_df) = c('start', 'end')
  }

  # omit columns for file groups not loaded
  return(dates_df)
}


