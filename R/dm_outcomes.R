library(magrittr)

config <- yaml::read_yaml("./config.yaml")

get_outcome <- function(fn = config$data$outcome$raw,
												delim = config$data$outcome$delim) {

	outcome <- readr::read_delim(file = fn,
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
									 file = config$data$outcome$proc)

	return(outcome)
}

