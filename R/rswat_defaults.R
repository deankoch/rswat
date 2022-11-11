

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
        c('\\.cellhru','gwflow')) |> as.data.frame() |> stats::setNames(c('pattern', 'type'))
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
                                  'loaded',
                                  'group',
                                  'n_table',
                                  'n_line',
                                  'n_var',
                                  'size')

# attributes to display in calls to rswat_find with trim=1
.rswat_gv_find_trim_1 = function() c('distance',
                                     'name',
                                     'field_num',
                                     'line_num',
                                     'file',
                                     'table')

# attributes to display in calls to rswat_find with trim=2
.rswat_gv_find_trim_2 = function() c('distance',
                                     'name',
                                     'file',
                                     'table')

# attributes to display in calls to rswat_find with trim=3
.rswat_gv_find_trim_3 = function() c('name',
                                     'file')

# simulation time step size codes (docs unclear on sub-hourly case, so it is not supported)
.rswat_gv_step_codes = function() c(daily = 0L,
                                    twice_daily = 1L,
                                    hourly = 24L,
                                    sub_hourly = NA_integer_)

