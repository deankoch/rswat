# helpers for rswat_db

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

  # initialize outputs
  out_msg = head(sta_files, 1L)
  out_msg[1L,] = rep('not loaded', length(sta_files))
  out_dates = rep(list(integer(2)), length(sta_files)) |> stats::setNames(names(out_msg))

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

#
rswat_weather_report = function(lazy=TRUE, .db=.rswat_db)
{
  f_sta = 'weather-sta.cli'
  if( !.db$is_file_loaded(f_sta) )
  {
    # attempt to load then return from not loaded case
    if(lazy) rswat(include=f_sta, quiet=TRUE, .db=.db)
    if( !.db$is_file_loaded(f_sta) ) return(paste0('• ', f_sta, ': not loaded\n'))
  }

  # collect the date ranges for each file group (or 0 for not loaded, or NA for simulated)
  dates_df = rswat_weather_dates(lazy=lazy, .db=.db)
  v_nm = names(dates_df)
  msg_stat = paste(nrow(.db$get_stor_df(f_sta)[[1L]]), 'stations')
  msg_prefix = paste0('✓ ', f_sta, ': ', msg_stat)

  # check for weather generator prompts and groups not yet loaded
  is_loaded = !sapply(dates_df, is.integer)
  is_simulated = sapply(dates_df, anyNA)

  # message about files not loaded
  msg_load = ifelse(any(!is_loaded),
                    yes = paste0('(', paste(v_nm[!is_loaded], collapse=', '), ' not loaded)'),
                    no = '')

  # message about weather data loaded (or simulated)
  if( any(is_loaded) )
  {
    msg_load = paste(msg_load, do.call(c, lapply(v_nm[is_loaded], \(nm) {

      item_prefix = paste0('\n            ✓ ', nm, ':')
      if( is_simulated[nm] ) return( paste(item_prefix, 'simulated') )

      # make a group label for printing
      item_range = paste( paste0('[', c(dates_df[c('start', 'end'), nm]), ']'), collapse=' to ' )
      paste(item_prefix, item_range)

    }) ), collapse='')
  }

  return(paste0(msg_prefix, ' ', msg_load, '\n'))
}

