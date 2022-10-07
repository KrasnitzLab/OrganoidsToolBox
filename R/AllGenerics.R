#' @title Is an object of class \code{DrugAUCQuantile}
#'
#' @description Functions to test inheritance relationships between an
#' object and class \code{DrugAUCQuantile}.
#'
#' @method is DrugAUCQuantile
#'
#' @param x an object.
#'
#' @param \ldots further arguments passed to or from other methods.
#'
#' @return a \code{logical}.
#'
#' @importFrom methods is
#' @export
is.DrugAUCQuantile <- function(x, ...) {
    inherits(x, "DrugAUCQuantile")
}


#' @title Is an object of class \code{DrugAUCQuantileNoReplicate}
#'
#' @description Functions to test inheritance relationships between an
#' object and class \code{DrugAUCQuantileNoReplicate}.
#'
#' @method is DrugAUCQuantileNoReplicate
#'
#' @param x an object.
#'
#' @param \ldots further arguments passed to or from other methods.
#'
#' @return a \code{logical}.
#'
#' @importFrom methods is
#' @export
is.DrugAUCQuantileNoReplicate <- function(x, ...) {
    inherits(x, "DrugAUCQuantileNoReplicate")
}
