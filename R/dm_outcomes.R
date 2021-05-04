library(magrittr)

config <- yaml::read_yaml("./config.yaml")

#' @title Download outcome data
#' @param fn_raw: File location from which to pull raw data
#' @param fn_proc: File location at which to store processed data
#' @param delim: Deliminator of raw data
get_outcome <- function(fn_raw = config$data$outcome$raw,
												fn_proc = config$data$outcome$proc,
												delim = config$data$outcome$delim) {

	outcome <- readr::read_delim(file = fn_raw,
															 delim = delim) %>%
		janitor::clean_names() %>%
		dplyr::filter(!is.na(county_code),
									!grepl(pattern = "Unidentified Counties",
												 x = county)) %>%
		dplyr::select(county,
									fips = county_code,
									deaths,
									births) %>%
		dplyr::mutate(death_rate = deaths/births*1000) %>%
		tidyr::separate(col = county,
										into = c("county", "state"),
										sep = ", ",
										remove = FALSE) %>%
		tidyr::pivot_longer(cols = -c(fips, county, state),
												names_to = "variable",
												values_to = "estimate") %>%
		dplyr::mutate(variable_name = variable)

	readr::write_csv(x = outcome,
									 file = fn_proc)

	return(outcome)
}

