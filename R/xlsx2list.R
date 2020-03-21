#' @name xlsx2list
#' 
#' @title Read xlsx book into a list
#' 
#' @description 
#' This function reads and format the data into a list, preparing it for export
#' into a relational database.
#' Data frames will be imported from an MS-Excel book by the function
#' [read.xlsx()].
#' 
#' @param file Character value indicating the path and/or the name of the file
#'     containing the data to be imported.
#' @param ... Further arguments (not yet used).
#' 
#' @return
#' A list with four data frames, namely `plots`, `species`, `individuals`, and
#' `records`.
#' 
#' @export xlsx2list
#' 
xlsx2list <- function(file, ...) {
	Plots <- read.xlsx(file, sheetName="plots", stringsAsFactors=FALSE)
	Species <- read.xlsx(file, sheetName="species", stringsAsFactors=FALSE)
	Records <- list()
	Dates <- list()
	for(i in Plots$plot_name) {
		Records[[i]] <- read.xlsx(file, sheetName=i, stringsAsFactors=FALSE,
				startRow=2, check.names=FALSE)
		Records[[i]]$plot_no <- with(Plots, plot_no[plot_name == i])
		Records[[i]]$taxon_code <- with(Species,
				taxon_code[match(Records[[i]]$taxon_name, taxon_name)])
		Dates[[i]] <- read.xlsx(file, sheetName=i, header=FALSE,
				stringsAsFactors=FALSE, rowIndex=1, check.names=FALSE)
	}
	Individuals <- lapply(Records, "[",
			i=c("individual_no","label_no","plot_no","taxon_code","dead"))
	Individuals <- do.call(rbind, Individuals)
	# Reference table for recors
	rec_vars <- data.frame(long_name=c("Height (cm)","BD (mm)", "DBH (mm)"),
			short_name=c("height_cm","bd_mm","dbh_mm"), stringsAsFactors=FALSE)
	records_list <- list()
	for(i in names(Records)) {
		date_idx <- which(colnames(Records[[i]]) %in% rec_vars$long_name)
		date_txt <- t(Dates[[i]][,paste(date_idx)])
		records_j <- list()
		for(j in unique(t(Dates[[i]][,paste(date_idx)]))) {
			rec_idx <- c(which(colnames(Records[[i]]) == "individual_no"),
					date_idx[date_txt == j])
			records_j[[j]] <- Records[[i]][,rec_idx]
			for(k in with(rec_vars, long_name[!long_name %in%
									colnames(records_j[[j]])]))
				records_j[[j]][,k] <- NA
			records_j[[j]]$record_date <- as.Date(j)
			records_j[[j]]$record_id <- NA
			records_j[[j]] <- records_j[[j]][,c("record_id","individual_no",
							"record_date", rec_vars$long_name)]
		}
		records_list[[i]] <- do.call(rbind, records_j)
	}
	records_list <- do.call(rbind, records_list)
	colnames(records_list) <- with(rec_vars, {
				temp <- colnames(records_list)
				for(i in long_name)
					temp[temp == i] <- short_name[long_name == i]
				temp
			})
	records_list <- records_list[apply(records_list[,rec_vars$short_name], 1,
					function(x) !all(is.na(x))),]
	records_list$record_id <- c(1:nrow(records_list))
	rownames(records_list) <- NULL
	# Set classes
	Species$taxon_id <- as.integer(Species$taxon_id)
	Individuals$individual_no <- as.integer(Individuals$individual_no)
	records_list$individual_no <- as.integer(records_list$individual_no)
	## for(i in rec_vars$short_name)
	##     records_list[,i] <- as.numeric(records_list[,i])
	# Output
	return(list(plots=Plots, species=Species, individuals=Individuals,
					records=records_list))
}
