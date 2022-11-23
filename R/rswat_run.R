
#'
#' Copy SWAT+ config files or make a backup of a project
#'
# When not supplied, `from` is set to the currently loaded SWAT+ project directory (usually
# ".../TxtInOut") and when `fname` is not supplied it is assigned all config files listed by
# `rswat_cio()`. The special argument `fname='.'` also copies any non-config files found in
# `from` (such as weather input files), but excludes directories.
#
# When neither `to` nor `from `is supplied, `from` is set as above, and `to` is set to
# `from`/rswat_backup_<foo>, where <foo> is a random string that is very unlikely to collide
# with any previous backups. This makes it easy to create a backup; just call `rswat_copy()`
# without arguments.
#
# When `from` but not `to` is supplied, `to` is set to the currently loaded SWAT+ project
# directory. This allows a backup to be restored by calling `rswat_copy(from=...)`. Files
# modified by `rswat_copy` are reloaded only if they have been loaded already in the R
# session.
#'
#' @param from character, path to the source directory
#' @param to character, path to the destination directory
#' @param fname character vector, name of files in `from` to copy
#' @param overwrite logical, whether to overwrite any existing files in path `to`
#' @param quiet logical, suppresses console messages
#'
#' @return a vector of paths to the files copied in their new location
#' @export
#'
rswat_copy = function(from=NULL, to=NULL, fname=NULL, overwrite=FALSE, quiet=FALSE)
{
  # # grab project directory from package environment
  # if( exists('.rswat') )
  # {
  #   # if no SWAT+ project loaded, attempt to load source directory `from`
  #   if( is.null(.rswat$ciopath) ) rswat_cio(from)
  #
  #   # copy the path from the package environment
  #   ciopath = .rswat$ciopath
  #
  #   # handle missing ciopath then assign (SWAT+ project) config files directory
  #   if( is.null(ciopath) ) stop('ciopath not found. Try setting it with `rswat_cio`')
  #   textio = dirname(ciopath)
  #
  # } else { stop('"file.cio" not found. Run `rswat_cio` to set its path') }
  #
  # # grab current config files list
  # cio = .rswat$cio
  #
  # # set default source and destination paths as needed
  # to.default = file.path(textio, paste0('_rswat_backup_', basename(tempfile()) ) )
  # if( is.null(from) ) from = textio
  # if( is.null(to) )
  # {
  #   # default destination is either textio or a subdirectory, depending on `from`
  #   if( from == textio ) to = to.default
  #   if( from != textio ) to = textio
  # }
  #
  # # default files list includes everything listed in 'file.cio' (and the file itself)
  # if( is.null(fname) ) fname = c('file.cio', cio$file)
  #
  # # special argument '.' copies all files
  # if( all(fname=='.') ) fname = list.files(from, include.dirs=FALSE)
  #
  # # define source and destination file paths and check for existing ones
  # dest.path = file.path(to, fname)
  # dest.exists = file.exists(dest.path)
  # src.path = file.path(from, fname)
  # src.exists = file.exists(src.path)
  #
  # # check for and fix missing files listed in `fname`
  # if( !all(src.exists) )
  # {
  #   # update the source and destination file lists to remove missing items
  #   fname = fname[src.exists]
  #   src.path = src.path[src.exists]
  #   src.exists =  src.exists[src.exists]
  #   dest.path = dest.path[src.exists]
  #   dest.exists = dest.exists[src.exists]
  # }
  #
  # # make list of existing destination files (possibly empty)
  # fname.overwrite = fname[dest.exists]
  #
  # # handle existing files when overwrite not enabled
  # if( ( !overwrite ) & ( length(fname.overwrite) > 0 ) )
  # {
  #   # warn of overwrites requested
  #   msg1 = 'the following files already exist in directory'
  #   msg2 = '(change `to` or set `overwrite=TRUE` to overwrite):\n'
  #   warning(paste(msg1, to, msg2, paste(fname.overwrite, collapse=', ')))
  #
  #   # update the source and destination file lists to remove conflicts
  #   idx.overwrite = fname %in% fname.overwrite
  #   fname = fname[!idx.overwrite]
  #   src.path = src.path[!idx.overwrite]
  #   src.exists =  src.exists[!idx.overwrite]
  #   dest.path = dest.path[!idx.overwrite]
  #   dest.exists = dest.exists[!idx.overwrite]
  #   fname.overwrite = character(0)
  # }
  #
  # # count number of files to be written
  # n.tocopy = length(fname)
  #
  # # error if we are left with nothing to write, then create directory as needed
  # if( n.tocopy == 0 ) stop('Nothing to write')
  # my_dir(to)
  #
  # # copy the files in a loop
  # if( !quiet ) cat( paste('copying', n.tocopy, 'file(s) to', basename(to), '...\n') )
  # if( !quiet ) pb = txtProgressBar(max=n.tocopy, style=3)
  # for(idx.file in 1:n.tocopy)
  # {
  #   file.copy(src.path[idx.file], dest.path[idx.file], overwrite=overwrite)
  #   if( !quiet ) setTxtProgressBar(pb, idx.file)
  # }
  # if( !quiet ) close(pb)
  #
  # # reload any config files that were replaced in current SWAT+ project directory
  # if( overwrite & ( length(fname.overwrite) > 0 ) & (to == textio) )
  # {
  #   # handle 'file.cio' replacements by refreshing files list
  #   if('file.cio' %in% fname.overwrite)
  #   {
  #     # maintain ignored files list
  #     ignore = cio %>% filter(ignored) %>% pull(file)
  #     cio = rswat_cio(ciopath, trim=FALSE, ignore=ignore, quiet=TRUE)
  #   }
  #
  #   # check for overwrites that coincide with currently loaded files and reload them (if any)
  #   fname.loaded = cio %>% filter( !is.na(ntab) ) %>% filter( !ignored )
  #   idx.coinc = fname.overwrite %in% fname.loaded
  #   if( any(idx.coinc) )
  #   {
  #     if( !quiet ) cat( paste('reloading', sum(idx.coinc), 'file(s)...') )
  #     rswat_open(fname.overwrite[idx.coinc], reload=TRUE, quiet=TRUE)
  #   }
  # }
  #
  # # end the console messages and return the paths to the files written
  # if( !quiet ) cat('done\n')
  # return(dest.path)
}
