#' @name rate_calc
#' 
#' @title Calculation of growth rates
#' 
#' @description 
#' Rates of increase of variables specified in argument `vars` will be
#' calculated in function of the variable `time` and the identity of observed
#' objects specified in argument `by`.
#' 
#' @param vars Character vector indicating the variables in `data` that will be
#'     assessed.
#' @param by Character value indicating the variable in `data` containing the
#'     identity of observed objects.
#' @param time Character value indicating the variable in `data` containing the
#'     time progress.
#' @param data A data frame containing the mentioned variables.
#' @param prefix A character value indicating a prefix added to the assessed
#'     variable in the output.
#' @param suffix A character value used as suffix.
#' @param na.rm A logical value indicating whether missing values in the
#'     assessed variable should be removed before doing the calculation or not.
#' @param ... Further variables (not yet in use).
#' 
#' @return Data frame `data` with added variables including calculated rates.
#' 
#' @export rate_calc
#' 
rate_calc <- function(vars=c("height_cm","bd_mm","dbh_mm"), by="individual_no",
		time="record_date", data, prefix="r_", suffix="", na.rm=TRUE, ...) {
	if(any(duplicated(data[,c(by, time)])))
		stop("It is not possible to calculate rates with repeated records.")
	for(i in vars) {
		new_var <- paste0(prefix, i, suffix)
		data2 <- data[order(data[,time]),c(by, time, i)]
		if(na.rm)
			data2 <- data2[!is.na(data2[,i]),]
		data2 <- split(data2, data2[,by])
		data2 <- lapply(data2, function(data, time, variable, new_var) {
					data$".diff_time" <- c(NA, diff(data[,time]))
					data[,variable] <- c(NA, diff(data[,variable]))
					data[,new_var] <- with(data, get(variable)/.diff_time)
					return(data)
				}, time=time, variable=i, new_var=new_var)
		data2 <- do.call(rbind, data2)
		data[,new_var] <- data2[match(with(data,
								paste(get(time), get(by), sep=".")),
						with(data2, paste(get(time), get(by), sep="."))),
				new_var]
	}
	return(data)
}
