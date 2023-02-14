# defaults for rswat functions

# TODO add backup detector

# keyword as shorthand for a useful group of file types to load
.rswat_gv_include_lu = function(include) switch(include,
                                                'basic' = c('simulation', 'climate'),
                                                'default' = 'config',
                                                'more' = c('weather', .rswat_gv_include_lu('normal')),
                                                'all' = c('output', .rswat_gv_include_lu('full')),
                                                include)

# aliases for date fields in output files
.rswat_gv_date_aliases = function() list(jday=c('day'), month=c('mon'), year=c('yr'))

# characters for show method of rswat_db
#.rswat_ok_char = function() c(yes='\U25B7', fail='\U25C9', no='\U25B6', sub='\U25B9')
#.rswat_ok_char = function() c(yes='\U25C6', fail='!', no='\U25C7', sub='\U25B9')
.rswat_ok_char = function() c(yes='\U2192',
                              sub='\U2937',
                              no='\U25CB',
                              fail='!')

# this number is used to denote NA in the weather input files
.rswat_gv_weather_NA_val = function() -99

# regex strings to use for organizing SWAT+ files into groups (called in rswat_scan_dir)
.rswat_gv_type_lu = function() {

  rbind(c('^.rswat_backup', 'backup'),
        c('file\\.cio', 'config'),
        c('object\\.prt', 'config'),
        c('\\.input', 'gwflow'),
        c('\\.rivcells', 'gwflow'),
        c('\\.hrucell','gwflow'),
        c('\\.cellhru','gwflow'),
        c('gwflow[[:punct:]]', 'gwflow'),
        c('\\.pcp','weather'),
        c('\\.tmp','weather'),
        c('\\.wnd','weather'),
        c('\\.hmd','weather'),
        c('\\.slr','weather'),
        c('\\.txt', 'output'),
        c('\\.ohg', 'output'),
        c('_warnings','log'),
        c('\\.fin','log'),
        c('\\.out','log')) |> as.data.frame() |> stats::setNames(c('pattern', 'type'))
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


# column order for rswat_files
.rswat_gv_cio_show = function() c('file',
                                  'group',
                                  'type',
                                  'groups', # plural case used by rswat_summary
                                  'files',
                                  'n_line',
                                  'n_var',
                                  'n_table',
                                  'size',
                                  'modified',
                                  'known',
                                  'loaded',
                                  'exists')

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


# .rswat_gv_line_num_adj = function(f, skip) {
#
#   if(f == 'crop_yld_aa.txt') return(ifelse(skip, 3L, 2L))
#   if(f == 'lu_change_out.txt') return(ifelse(skip, 0L, -1L))
#   if( endsWith(tolower(f), '.ohg') ) return(ifelse(skip, -1L, -2L))
#   return(0L)
# }


# # adjustments (+/-) to the line numbers defined next
# .rswat_gv_line_num_adj = function(f, skip) { switch(f,
# 'crop_yld_aa.txt' = ifelse(skip, 3L, 2L),
# 'lu_change_out.txt' = ifelse(skip, 0L, -1L),
# 0L)
# }


# line number of first data row (skip=F), or the number of lines to ignore at the start
.rswat_gv_line_num = function(type, f='', skip=FALSE) switch(type,

   weather = ifelse(skip, 1L, 4L),
   output = ifelse(skip, 1L, 4L) + .rswat_gv_line_num_adj(f, skip),
   ifelse(skip, 1L, 3L)
)

# adjustments (+/-) to the line numbers defined above
.rswat_gv_line_num_adj = function(f, skip) {

  if( endsWith(f, '.ohg') ) return( ifelse(skip, -1L, -2L) )
  if( f == 'crop_yld_aa.txt' ) return( ifelse(skip, 3L, 2L) )
  if( f == 'lu_change_out.txt' ) return( ifelse(skip, 0L, -1L) )
  return(0L)
}

