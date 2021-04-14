library(magrittr)

setwd("/Users/Prayag/Documents/Rice/STAT 410/STAT 410/final_project")

config <- yaml::read_yaml("./config.yaml")

get_outcome <- function(fn = config$data$outcome$pth,
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
										remove = FALSE)

	readr::write_csv(x = outcome,
									 file = "data/outcome_processed.csv")
}

outcome <- get_outcome()
