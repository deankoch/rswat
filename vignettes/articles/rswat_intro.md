Getting started with rswat
================
Dean Koch
July 27, 2023

- [Introduction](#introduction)
  - [Getting started](#getting-started)
  - [Exploring SWAT+](#exploring-swat)
    - [File index](#file-index)
    - [Search](#search)
    - [Documentation](#documentation)
- [Editing](#editing)
  - [Open a file](#open-a-file)
  - [Modify parameters](#modify-parameters)
  - [Project state](#project-state)
- [Simulations](#simulations)

# Introduction

This article explains how to use rswat to explore and manage a SWAT+
project. It picks up where the [Lamar
River](https://github.com/deankoch/rswat.maker/blob/master/vignettes/articles/lamar.md)
tutorial from [`rswat.maker`](https://github.com/deankoch/rswat.maker)
left off, showing how rswat can be used to carry out simulations and
calibrate a SWAT+ model. Our example is a small headwater catchment of
Soda Butte Creek, near the Silver Gate YNP boundary.

<img src="rswat_intro_files/figure-gfm/source-project-1.png" style="display: block; margin: auto;" />

## Getting started

Users will need an existing SWAT+ project. This is a directory, usually
called “TxtInOut”, with several dozen plaintext config files. It can be
hard to track changes with so many files, so we recommend always working
with backups, and provide helper functions `rswat_backup` and
`rswat_restore` to make this easier. Pass `rswat_restore` a source
directory (`txt_dir`) to make a copy in a new directory (`dest_dir`).

``` r
basename(txt_dir)
#> [1] "TxtInOut"
basename(dest_dir)
#> [1] "soda_test"
txt_dir |> rswat_restore(dest_dir, overwrite=TRUE) |> head()
#> copying 222 file(s)
#> # A tibble: 6 × 4
#>   file            overwritten added completed
#>   <chr>           <lgl>       <lgl> <lgl>    
#> 1 aqu_catunit.ele FALSE       TRUE  TRUE     
#> 2 aqu_dr.swf      FALSE       TRUE  TRUE     
#> 3 aquifer.aqu     FALSE       TRUE  TRUE     
#> 4 aquifer.con     FALSE       TRUE  TRUE     
#> 5 aquifer_aa.txt  FALSE       TRUE  TRUE     
#> 6 aquifer_yr.txt  FALSE       TRUE  TRUE
```

Load any SWAT+ project by passing its path to `rswat`. The code below
loads the copy created above.

``` r
rswat(dest_dir, quiet=TRUE, exe_path=swat_path)
```

In this example we set `quiet=TRUE` to disable a progress bar (which
doesn’t render well in markdown) and point `exe_path` to our local copy
of the SWAT+ simulator, for later use.

## Exploring SWAT+

Once the project directory is assigned, subsequent calls to `rswat` with
default arguments print summary information about the project.

``` r
rswat()
#> all requested files loaded
#> rswat summary
#> →  simulator: C:/SWAT/SWATPlus/SWATPlusEditor/resources/app.asar.unpacked/static/swat_exe/rev60.5.7_64rel.exe
#> →  directory: D:/rswat_data/swat/soda_test
#>    --------- 
#>  ⤷  file.cio: lists 63 config files in 19 groups 
#>    ⤷ climate: 15 stations in weather-sta.cli
#>        ⤷ wgn: simulating none
#>        ○ sta: pcp, tmp, slr, hmd, wnd (none loaded) 
#>  ⤷  time.sim: [ 2023-07-16 to 2023-07-23 ]
#> !  print.prt: invalid dates! Check them with rswat_open("print.prt")
```

The “all requested files loaded” refers to arguments `include` and
`exclude` whose default settings skip loading certain large files, like
weather inputs, until they are needed. Read more about this in `?rswat`.

### File index

Get a list of all files in your SWAT+ project (loaded or not) using
`rswat_files`

``` r
rswat_files()
#> 222 files in D:/rswat_data/swat/soda_test
#> # A tibble: 222 × 10
#>    file           group      type   n_line n_var n_table        size modified            known loaded
#>    <chr>          <chr>      <chr>   <int> <int>   <int> [kilobytes] <dttm>              <lgl> <lgl> 
#>  1 file.cio       cio        config     29    18       1       3.4   2023-07-27 19:20:14 TRUE  TRUE  
#>  2 object.cnt     simulation config      1    21       1       0.674 2023-07-27 19:20:14 TRUE  TRUE  
#>  3 print.prt      simulation config     42    19       5       3.39  2023-07-27 19:20:14 TRUE  TRUE  
#>  4 time.sim       simulation config      1     5       1       0.169 2023-07-27 19:20:14 TRUE  TRUE  
#>  5 codes.bsn      basin      config      1    24       1       0.598 2023-07-27 19:20:14 TRUE  TRUE  
#>  6 parameters.bsn basin      config      1    44       1       1.3   2023-07-27 19:20:14 TRUE  TRUE  
#>  7 hmd.cli        climate    config     15     1       1       0.248 2023-07-27 19:20:14 TRUE  TRUE  
#>  8 pcp.cli        climate    config     15     1       1       0.259 2023-07-27 19:20:14 TRUE  TRUE  
#>  9 slr.cli        climate    config     15     1       1       0.291 2023-07-27 19:20:14 TRUE  TRUE  
#> 10 tmp.cli        climate    config     15     1       1       0.257 2023-07-27 19:20:14 TRUE  TRUE  
#> # ℹ 212 more rows
```

This can be filtered using a number of categories (read more about this
in `?rswat_files`)

``` r
rswat_files('climate')
#> 7 files in D:/rswat_data/swat/soda_test
#> # A tibble: 7 × 10
#>   file            group   type   n_line n_var n_table        size modified            known loaded
#>   <chr>           <chr>   <chr>   <int> <int>   <int> [kilobytes] <dttm>              <lgl> <lgl> 
#> 1 hmd.cli         climate config     15     1       1       0.248 2023-07-27 19:20:14 TRUE  TRUE  
#> 2 pcp.cli         climate config     15     1       1       0.259 2023-07-27 19:20:14 TRUE  TRUE  
#> 3 slr.cli         climate config     15     1       1       0.291 2023-07-27 19:20:14 TRUE  TRUE  
#> 4 tmp.cli         climate config     15     1       1       0.257 2023-07-27 19:20:14 TRUE  TRUE  
#> 5 weather-sta.cli climate config     15     9       1       3.41  2023-07-27 19:20:14 TRUE  TRUE  
#> 6 weather-wgn.cli climate config    298   173      13      31.9   2023-07-27 19:20:14 TRUE  TRUE  
#> 7 wnd.cli         climate config     15     1       1       0.271 2023-07-27 19:20:14 TRUE  TRUE
```

### Search

Once a file has been loaded, it becomes searchable with `rswat_find`.
This supports fuzzy matching to variable names, to help with typos and
names that have changed over time.

For example the method for modelling potential evapotranspiration (PET),
formally called ‘IPET’ in SWAT2012, is now called ‘pet’. Searching for
the old name turns up no exact matches. However users can cast a wider
net by increasing `fuzzy` (see `?rswat_find` to learn more). With
`fuzzy=2` we find the new name, plus one false positive

``` r
rswat_find(pattern='IPET', fuzzy=2)
#> 2 result(s) for "IPET" in 2 file(s) (searched 61)
#> looking up definitions...
#> # A tibble: 2 × 8
#>   file        group   table class     name  alias        match desc                                        
#>   <chr>       <chr>   <dbl> <chr>     <chr> <chr>        <chr> <chr>                                       
#> 1 codes.bsn   basin       1 integer   pet   ""           ***   Potential evapotranspiration (PET) method. …
#> 2 initial.cha channel     1 character pest  "pesticides" *     Pesticides initialization in channel (point…
```

Repeating the call with `fuzzy=1` produces the unique result ‘pet’. We
can see that this variable is stored in the file ‘codes.bsn’. Pass a
file name in `pattern` to report on all the variables in the file

``` r
rswat_find(pattern='codes.bsn')
#> 24 parameters(s) found in codes.bsn
#> looking up definitions...
#> # A tibble: 24 × 8
#>    file      group table class     name      alias     match desc                                          
#>    <chr>     <chr> <dbl> <chr>     <chr>     <chr>     <chr> <chr>                                         
#>  1 codes.bsn basin     1 character pet_file  "petfile" **    Potential ET filename                         
#>  2 codes.bsn basin     1 character wq_file   "wwqfile" **    Watershed stream water quality filename       
#>  3 codes.bsn basin     1 integer   pet       ""        ***   Potential evapotranspiration (PET) method. Th…
#>  4 codes.bsn basin     1 integer   event     ""        ***   Rainfall/runoff/routing option: 0   daily rai…
#>  5 codes.bsn basin     1 integer   crack     "crk"     *     Crack flow code. There are two options: 0 do …
#>  6 codes.bsn basin     1 integer   swift_out "subwq"   *     Subbasin water quality code. The algorithms u…
#>  7 codes.bsn basin     1 integer   sed_det   ""        ***   Code governing calculation of daily maximum h…
#>  8 codes.bsn basin     1 integer   rte_cha   "rte"     **    Channel water routing method: 0 variable stor…
#>  9 codes.bsn basin     1 integer   deg_cha   "deg"     **    Channel degradation code. There are two optio…
#> 10 codes.bsn basin     1 integer   wq_cha    "wq"      **    In-stream water quality code. The variable id…
#> # ℹ 14 more rows
```

By default `rswat_find` will attempt to match all results to definitions
(the `desc` field) from the SWAT+ inputs documentation PDF. As there is
some uncertainty in matching old and new names automatically to text
scraped from a PDF, a (three-star) match confidence ranking is reported
by `rswat_find` along with any aliases. When alias is an empty string
the function has found an exact match, and the definition is very likely
to be correct.

### Documentation

`rswat` includes a plaintext copy of the SWAT+ documentation PDF to make
it more accessible in the R environment. Call `rswat_docs` to get
`tibble` of results for a search query. Searching for a file name will
usually pull up a full definitions list, along with information about
where in the PDF this text can be found.

``` r
rswat_docs('codes.bsn')
#> 24 definition(s) found for codes.bsn
#> # A tibble: 24 × 5
#>     page  line file      name    desc                                                                      
#>    <int> <int> <chr>     <chr>   <chr>                                                                     
#>  1    18     6 codes.bsn petfile Potential ET filename                                                     
#>  2    18     7 codes.bsn wwqfile Watershed stream water quality filename                                   
#>  3    18     8 codes.bsn pet     Potential evapotranspiration (PET) method. There are four options for pot…
#>  4    18    24 codes.bsn event   Rainfall/runoff/routing option: 0   daily rainfall/curve number runoff/da…
#>  5    18    29 codes.bsn crk     Crack flow code. There are two options: 0 do not model crack flow in soil…
#>  6    19     2 codes.bsn subwq   Subbasin water quality code. The algorithms used to calculate loadings of…
#>  7    19    15 codes.bsn sed_det Code governing calculation of daily maximum half-hour rainfall value: 0  …
#>  8    19    27 codes.bsn rte     Channel water routing method: 0 variable storage method 1 Muskingum metho…
#>  9    19    34 codes.bsn deg     Channel degradation code. There are two options: 0 channel dimensions are…
#> 10    20     2 codes.bsn wq      In-stream water quality code. The variable identifies whether in-stream t…
#> # ℹ 14 more rows
```

Variable names definitions can also be searched directly in
`rswat_docs`. For example searching again for ‘IPET’ turns another name
and location: ‘ipet’ in the ‘hru-lte.hru’ file.

``` r
rswat_docs('IPET')
#> "IPET" matched to 1 parameter(s) in 1 file(s)
#> # A tibble: 1 × 5
#>    page  line file        name  desc                                                                       
#>   <int> <int> <chr>       <chr> <chr>                                                                      
#> 1    88    19 hru-lte.hru ipet  Potential evapotranspiration (PET) method (character): ‘harg’ = Hargreaves…
```

Our example is not an “LTE”-type project, so don’t have ‘hru-lte.hru’ in
our project directory. `rswat_docs` will find the match, but
`rswat_find` (which searches only loaded files) will not.

# Editing

So far we have only browsed an existing project. The real power of
`rswat` is in allowing R users to directly modify SWAT+ config files on
disk, changing the parameters that are passed into the simulator.
Editing parameters with `rswat` should feel very natural to R users,
because `rswat` represents the all config files as data frames.

## Open a file

To open a file as a data frame, pass the file name to `rswat_open`. For
example, the ‘time.sim’ file controls the time period of simulations,
and consists of a single, one-line table

``` r
rswat_open('time.sim') |> str()
#> 'data.frame':    1 obs. of  5 variables:
#>  $ day_start: int 197
#>  $ yrc_start: int 2023
#>  $ day_end  : int 204
#>  $ yrc_end  : int 2023
#>  $ step     : int 0
#>  - attr(*, "rswat_path")= chr "D:/rswat_data/swat/soda_test"
#>  - attr(*, "rswat_fname")= chr "time.sim"
#>  - attr(*, "rswat_table_num")= int 1
```

Most SWAT+ config files have this one-table structure but there are a
few multi-table exceptions, like `print.prt`, which are returned as a
list of data frames. Below we print the first two of its tables

``` r
rswat_open('print.prt') |> head(2)
#> [[1]]
#>   nyskip day_start yrc_start day_end yrc_end interval
#> 1      1         0         0       0       0        1
#> 
#> [[2]]
#>   aa_int_cnt
#> 1          0
```

## Modify parameters

The ‘print.prt’ file a good place to start, since our `rswat()` call at
the beginning warned of malformed dates. In the output above we can see
that no start/end dates were specified for the simulator *print*out.

To change a parameter, simply get the data frame `rswat_open`, change
its values, and pass it back to `rswat_write`. In this example we will
extend the simulation period in ‘time.sim’, then set the dates in
‘print.prt’ to match.

``` r
# prepare to copy from time.sim to first table of print.prt
sim = rswat_open('time.sim')
prt = rswat_open('print.prt')[[1]]

# assign new values
nm_copy = c('day_start', 'yrc_start', 'day_end', 'yrc_end')
sim['yrc_start'] = sim['yrc_start'] - 2L
prt[nm_copy] = sim[nm_copy]
```

Pass this modified data frame to `rswat_write` without setting
`overwrite` to get a list of pending changes. Set `overwrite=TRUE` to
change the file on disk.

``` r
sim |> rswat_write(overwrite=TRUE)
#> 1 field(s) modified in time.sim
#> writing changes to D:/rswat_data/swat/soda_test/time.sim
#> # A tibble: 1 × 7
#>   file     name      table line_num field_num value replacement
#>   <chr>    <chr>     <dbl>    <int>     <int> <chr> <chr>      
#> 1 time.sim yrc_start     1        3         2 2023  2021
prt |> rswat_write(overwrite=TRUE)
#> 4 field(s) modified in print.prt
#> writing changes to D:/rswat_data/swat/soda_test/print.prt
#> # A tibble: 4 × 7
#>   file      name      table line_num field_num value replacement
#>   <chr>     <chr>     <dbl>    <int>     <int> <chr> <chr>      
#> 1 print.prt day_start     1        3         2 0     197        
#> 2 print.prt yrc_start     1        3         3 0     2021       
#> 3 print.prt day_end       1        3         4 0     204        
#> 4 print.prt yrc_end       1        3         5 0     2023
```

## Project state

Changes written with `rswat_write` will persist in the files on disk,
and in subsequent calls `rswat_open` will return the newest version.

``` r
rswat()
#> all requested files loaded
#> rswat summary
#> →  simulator: C:/SWAT/SWATPlus/SWATPlusEditor/resources/app.asar.unpacked/static/swat_exe/rev60.5.7_64rel.exe
#> →  directory: D:/rswat_data/swat/soda_test
#>    --------- 
#>  ⤷  file.cio: lists 63 config files in 19 groups 
#>    ⤷ climate: 15 stations in weather-sta.cli
#>        ⤷ wgn: simulating none
#>        ○ sta: pcp, tmp, slr, hmd, wnd (none loaded) 
#>  ⤷  time.sim: [ 2021-07-16 to 2023-07-23 ]
#>  ⤷ print.prt: [ 2022-07-16 to 2023-07-23 ]
#>    ⤷   daily: none
#>    ⤷ monthly: none
#>    ⤷  yearly: all
#>    ⤷   avann: all
```

Notice the printout date range shows the updated values instead of the
warning we saw earlier

# Simulations

To run simulations rswat uses `shell` to call the simulator executable.
We already set its path, in our first call to `rswat`, so we are ready
to go. Note that this doesn’t need to be assigned for most of the
package functions to work, so rswat should still function as an editor
without a working SWAT+ installation.

In the latest release the simulator file for Windows is called
‘rev60.5.7_64rel.exe’ and it can be found in the
‘SWAT/SWATPlus/SWATPlusEditor’ directory tree. Standalone versions can
also be downloaded if you are unsure which executable to use.

Once the path is set, call `rswat_exec()` to run the simulator and write
output to your project directory.

``` r
rswat_exec()
#> SWAT+ simulation finished in 7.54 seconds
#> 7 log and 64 output and 10 unknown files were written
#> # A tibble: 81 × 2
#>    file             type  
#>    <chr>            <chr> 
#>  1 area_calc.out    log   
#>  2 checker.out      log   
#>  3 diagnostics.out  log   
#>  4 erosion.out      log   
#>  5 files_out.out    log   
#>  6 simulation.out   log   
#>  7 success.fin      log   
#>  8 aquifer_aa.txt   output
#>  9 aquifer_yr.txt   output
#> 10 basin_aqu_aa.txt output
#> # ℹ 71 more rows
```

The function returns a list of output files written when it finishes.
Outputs an be viewed the same way as config files, using `rswat_files`
and `rswat_open` (but not `rswat_write`). `dplyr::tibble` is useful here
for printing the result in R, as these output tables can wide and very
long.

``` r
rswat_open('basin_wb_yr.txt') |> dplyr::tibble()
#> # A tibble: 2 × 47
#>    jday   mon   day    yr  unit gis_id name       precip snofall snomlt surq_gen  latq wateryld  perc    et
#>   <int> <int> <int> <int> <int>  <int> <chr>       <dbl>   <dbl>  <dbl>    <dbl> <dbl>    <dbl> <dbl> <dbl>
#> 1   365    12    31  2022     1      1 Drswat_da…  1183.    613.   615.     80.1  31.2    111.   92.0  362.
#> 2   204     7    23  2023     1      1 Drswat_da…   731.    422.   533.     79.5  17.9     97.4  53.9  108.
#> # ℹ 32 more variables: ecanopy <dbl>, eplant <dbl>, esoil <dbl>, surq_cont <dbl>, cn <dbl>, sw_init <dbl>,
#> #   sw_final <dbl>, sw_ave <dbl>, sw_300 <dbl>, sno_init <dbl>, sno_final <dbl>, snopack <dbl>, pet <dbl>,
#> #   qtile <dbl>, irr <dbl>, surq_runon <dbl>, latq_runon <dbl>, overbank <dbl>, surq_cha <dbl>,
#> #   surq_res <dbl>, surq_ls <dbl>, latq_cha <dbl>, latq_res <dbl>, latq_ls <dbl>, gwtranq <dbl>,
#> #   satex <dbl>, satex_chan <dbl>, sw_change <dbl>, lagsurf <dbl>, laglatq <dbl>, lagsatex <dbl>,
#> #   wet_out <dbl>
```

We provide a number of helper functions for managing outputs. For
example to get Date objects instead of Julian date integers, pipe the
output to `rswat_date_conversion`

``` r
# hide most columns for tidyness
rswat_open('basin_wb_yr.txt')[, seq(8)] |> rswat_date_conversion()
#>         date unit gis_id             name   precip
#> 1 2022-12-31    1      1 Drswat_datalamar 1182.762
#> 2 2023-07-23    1      1 Drswat_datalamar  730.612
```

`rswat_open`, `rswat_write`, and `rswat_exec` is a powerful combination.
It allows users to programmatically control virtually all aspects of a
SWAT+ simulation. In the next vignette we show some examples, by
calibrating a forecasting model for the Soda Butte from this example.
