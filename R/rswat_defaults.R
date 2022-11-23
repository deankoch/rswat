# defaults for rswat functions

# line number of the first data row in the weather input files, and the number used for NA
.rswat_gv_weather_line_num = function() 4L
.rswat_gv_weather_NA_val = function() -99

# regex strings to use for organizing SWAT+ files into groups (called in rswat_scan_dir)
.rswat_gv_type_lu = function() {

  rbind(c('file\\.cio', 'config'),
        c('\\.pcp','weather'),
        c('\\.tmp','weather'),
        c('\\.wnd','weather'),
        c('\\.hmd','weather'),
        c('\\.slr','weather'),
        c('\\.txt', 'output'),
        c('_warnings','log'),
        c('\\.fin','log'),
        c('\\.out','log'),
        c('\\.input', 'gwflow'),
        c('\\.rivcells', 'gwflow'),
        c('\\.hrucell','gwflow'),
        c('\\.cellhru','gwflow'),
        c('gwflow[[:punct:]]', 'gwflow')) |> as.data.frame() |> stats::setNames(c('pattern', 'type'))
}

# config files using "y" and "n" to represent logical (called in rswat_scan_txt, rswat_rtable_txt)
.rswat_gv_logical_as_yn = function() {

  c('print.prt',
    'flo_con.dtl',
    'lum.dtl',
    'res_rel.dtl',
    'scen_lu.dtl')
}

# files to ignore in default rswat_load(). Note that 'log' type files are always ignored
.rswat_gv_exclude = function() {

  c('decision_table',
    'output',
    'gwflow')
}


# attributes to display in default calls to rswat_files
.rswat_gv_cio_show = function() c('file',
                                  'type',
                                  'group',
                                  'loaded',
                                  'n_table',
                                  'n_line',
                                  'n_var',
                                  'size')

.rswat_gv_match_docs_trim = function(trim) switch(trim,

  '5' = c('name', 'alias', 'id_alias', 'match_distance', 'match', 'desc'),
  '4' = c('name', 'alias', 'match_distance', 'match', 'desc'),
  '3' = c('name', 'alias', 'match', 'desc'),
  '2' = c('name', 'alias', 'desc'),
  '1' = c('name','alias'),
)

# attributes to display search results with trim
.rswat_gv_find_trim = function(trim) switch(trim,

  '5' = c('file', 'type', 'group', 'table', 'field_num', 'line_num', 'class', 'n_prec', 'name'),
  '4' = c('file', 'type', 'group', 'table', 'class', 'name'),
  '3' = c('file', 'group', 'name'),
  '2' = c('file', 'name'),
  '1' = c('name')

)

# simulation time step size codes (docs unclear on sub-hourly case, so it is not supported)
.rswat_gv_step_codes = function() c(daily = 0L,
                                    twice_daily = 1L,
                                    hourly = 24L,
                                    sub_hourly = NA_integer_)


# default costs for base::adist in rswat_string_dist
.rswat_gv_costs = function() list(ins=3, del=1, sub=2)

# variables names to omit from documentation search in rswat_match_docs
.rswat_gv_nm_nomatch = function() c('plantnm', 'name', 'id', 'description')
