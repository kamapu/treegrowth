#' @name odb2df
#' 
#' @title Import ODB database into a single data frame
#' 
#' @description 
#' ODB files written by [list2odb()] will be queried into a single data frame.
#' 
#' @param file Character value indicating path and/or name of ODB file to be
#'     imported.
#' @param ... Further arguments (not yet in use).
#' 
#' @return A data frame.
#' 
#' @export odb2df
#' 
odb2df <- function(file, ...) {
	conn <- odb.open(file)
	# For assessment we will get everything in one table
	Query <- paste0('SELECT *\n',
			'FROM "individuals"\n',
			'JOIN "records" ',
			'ON "individuals"."individual_no"="records"."individual_no"\n',
			'JOIN "plots" ON "plots"."plot_no"="individuals"."plot_no"\n',
			'JOIN "species" ',
			'ON "species"."taxon_code"="individuals"."taxon_code"')
	Data <- odb.read(conn, Query)
	odb.close(conn)
	for(i in c("dead","record_date","planting_date"))
		Data[,i] <- as.Date(Data[,i])
	return(Data)
}
