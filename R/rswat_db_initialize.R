#' Check that the rswat package was loaded properly and .rswat_db was found
#'
#' When the `rswat` package is loaded, an object named `.rswat_db` is defined and
#' (by default) passed to function calls through argument `.db`. This stores information
#' about the SWAT+ project as a reference class object that can be accessed globally
#' (see `?rswat_db`). Do not modify (or copy) `.rswat_db` unless you know what you are
#' doing.
#'
#' This function checks that `.rswat_db` exists and is of the correct class. It returns
#' nothing but has the side effect (when successful) of printing information about
#' `.rswat_db` to the console (unless `quiet=TRUE`).
#'
#' @include rswat_db.R rswat_db_helpers.R
#' @include rswat.R rswat_utils.R rswat_defaults.R
#' @include rswat_db_config_methods.R rswat_db_config_methods_helpers.R
#' @include rswat_write.R
#' @include rswat_find.R
#' @include rswat_run.R
#'
#' @param quiet logical, indicates to suppress console messages
#' @param .db rswat_db object, for internal use
#'
#' @return nothing
#' @export
#'
#' @examples
#' rswat_check()
rswat_check = function(quiet=FALSE, .db=.rswat_db)
{
  msg_db_problem = 'rswat database not found. Try running `.rswat_db = rswat_db$new()`'
  if( !exists('.db') | !is(.db, 'rswat_db')) stop(msg_db_problem)
  if(!quiet) .db$show()
}

#' Put this file last in the load order!
#'
#' The code below defines an rswat_db reference class object to store data and information
#' about the currently loaded SWAT+ project. The class, its methods, and any dependent
#' functions must be defined first. Make sure that all R files appear in the 'include' tags
#' above. This ensures the R files are collated in the right order.

.rswat_db = rswat_db$new()
rswat_check(quiet=TRUE)

