

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

  c('print.prt', 'flo_con.dtl', 'lum.dtl', 'res_rel.dtl', 'scen_lu.dtl')
}

# files to ignore in default rswat_load(). Note that 'log' type files are always ignored
.rswat_gv_ignore = function() {

  c('decision_table', 'output', 'gwflow')
}
