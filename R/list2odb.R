#' @name list2odb
#' 
#' @title Write ODB file from formatted list
#' 
#' @description 
#' The list produced by [xlsx2list()] will be written in a relational database
#' as ODB file (for OpenOffice or LibreOffice).
#' 
#' @param list A list imported by [xlsx2list()].
#' @param file Character value indicating the path and/or file name of resulting
#'     ODB file.
#' @param ... Further arguments (not yet in use).
#' 
#' @return An ODB file.
#' 
#' @export list2odb
#' 
list2odb <- function(list, file, ...) {
	# Creating database
	odb.create(file)
	conn <- odb.open(file)
	# 1: plots
	nc_plot_no <- with(list$plots, max(nchar(plot_no)))
	nc_plot_name <- with(list$plots, max(nchar(plot_name)))
	Query <- paste0('CREATE TABLE "plots" (\n',
			'"plot_no" VARCHAR(', nc_plot_no, ') PRIMARY KEY,\n',
			'"plot_name" VARCHAR(', nc_plot_name,'),\n',
			'"planting_date" DATE,\n',
			'"size_m2" INTEGER\n',
			')\n')
	odb.write(conn, Query)
	odb.comments(conn, "plots", "plot_no") <- "Identifier for plot."
	odb.comments(conn, "plots", "plot_name") <- "Short name for plot."
	odb.comments(conn, "plots", "planting_date") <- "Date of transplanting."
	odb.comments(conn, "plots", "size_m2") <- "Plot size in square meters."
	odb.insert(conn, '"plots"', list$plots)
	# 2: species
	nc_taxon_code <- with(list$species, max(nchar(taxon_code)))
	nc_taxon_name <- with(list$species, max(nchar(taxon_name)))
	nc_author_name <- with(list$species, max(nchar(author_name)))
	nc_genus <- with(list$species, max(nchar(genus)))
	nc_family <- with(list$species, max(nchar(family)))
	Query <- paste0('CREATE TABLE "species" (\n',
			'"taxon_code" VARCHAR(', nc_taxon_code, ') PRIMARY KEY,\n',
			'"taxon_id" INTEGER,\n',
			'"taxon_name" VARCHAR(', nc_taxon_name, '),\n',
			'"author_name" VARCHAR(', nc_author_name, '),\n',
			'"genus" VARCHAR(', nc_genus, '),\n',
			'"family" VARCHAR(', nc_family, ')\n',
			')\n')
	odb.write(conn, Query)
	odb.comments(conn, "species", "taxon_code") <- "Identifier of taxon."
	odb.comments(conn, "species", "taxon_id") <-
			"Identifier of taxon in SWEA-Dataveg."
	odb.comments(conn, "species", "taxon_name") <- "Full name of taxon."
	odb.comments(conn, "species", "author_name") <- "Author of taxon name."
	odb.comments(conn, "species", "genus") <- "Name of respective genus."
	odb.comments(conn, "species", "family") <- "Name of respective family."
	odb.insert(conn, '"species"', list$species)
	# 3: individuals
	nc_label_no <- with(list$individuals, max(nchar(label_no)))
	Query <- paste0('CREATE TABLE "individuals" (\n',
			'"individual_no" INTEGER PRIMARY KEY,\n',
			'"label_no" VARCHAR(', nc_label_no, '),\n',
			'"plot_no" VARCHAR(', nc_plot_no, '),\n',
			'"taxon_code" VARCHAR(', nc_taxon_code, '),\n',
			'"dead" DATE,\n',
			'FOREIGN KEY ("plot_no") REFERENCES "plots" ("plot_no") ',
			'ON DELETE CASCADE,\n',
			'FOREIGN KEY ("taxon_code") REFERENCES "species" ("taxon_code") ',
			'ON DELETE CASCADE\n',
			')\n')
	odb.write(conn, Query)
	odb.comments(conn, "individuals", "individual_id") <-
			"Identifier for individual tree."
	odb.comments(conn, "individuals", "label_no") <-
			"Current label attached to the respective tree."
	odb.comments(conn, "individuals", "plot_no") <-
			"Respective plot id (foreign key)."
	odb.comments(conn, "individuals", "taxon_code") <-
			"Code of respective species (foreign key)."
	odb.comments(conn, "individuals", "dead") <-
			"In the case of dead individual, date of recorded death."
	odb.insert(conn, '"individuals"', list$individuals)
	# 4: records
	Query <- paste0('CREATE TABLE "records" (\n',
			'"record_id" INTEGER PRIMARY KEY,\n',
			'"individual_no" INTEGER,\n',
			'"record_date" DATE,\n',
			'"height_cm" NUMERIC,\n',
			'"bd_mm" NUMERIC,\n',
			'"dbh_mm" NUMERIC,\n',
			'FOREIGN KEY ("individual_no") REFERENCES "individuals" ',
			'("individual_no") ON DELETE CASCADE\n',
			')\n')
	odb.write(conn, Query)
	odb.comments(conn, "records", "record_id") <-
			"Identifier for individual record."
	odb.comments(conn, "records", "individual_no") <-
			"Identifier for individual tree (foreign key)."
	odb.comments(conn, "records", "record_date") <- "Sampling date."
	odb.comments(conn, "records", "height_cm") <- "Height of tree (in cm)."
	odb.comments(conn, "records", "bd_mm") <- "Basal diameter (in mm)."
	odb.comments(conn, "records", "dbh_mm") <-
			"Diameter at breast height (in mm)."
	odb.insert(conn, '"records"', list$records)
	# End
    odb.close(conn)
	message("DONE")
}
