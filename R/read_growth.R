#' @name read_growth
#' 
#' @title Read growth data and produce an ODB file
#' 
#' @description 
#' Read row data and export to a single data frame using an ODB file as
#' intermediating file.
#' 
#' This function applies the sequence [xlsx2list()] -> [list2odb()] ->
#' [odb2df()].
#' 
#' @param xlsx Character value with the path and/or name of the XLSX input file.
#' @param odb Character value indicating the path and/or name of output ODB
#'     file.
#' @param format_date Character value indicating format of date entries (see
#'     [xlsx2list()]).
#' @param ... Further arguments (not yet in use).
#' 
#' @return A data frame and an ODB file as in [odb2df()].
#' 
#' @export read_growth
#' 
read_growth <- function(xlsx, odb=".temp.odb", format_date="%d.%m.%Y", ...) {
	DB <- xlsx2list(xlsx, format_date)
	list2odb(DB, odb)
	DB <- odb2df(odb)
	return(DB)
}
