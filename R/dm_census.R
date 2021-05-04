library(magrittr)

config <- yaml::read_yaml("./config.yaml")

#' @title Download the raw census data
#' @param variables: The census variables to download
#' @param fn_raw: File location at which to store raw data
download_census <- function(variables, fn_raw) {

	df <- tidycensus::get_acs(geography = "county", variables = variables)
	readr::write_csv(x = df,
									 file = fn_raw)

	return(df)
}

#' @title Clean the census data
#' @param df: Data
#' @param census_vars: Variable lookup table
#' @param aggregate_categories: For combining data
clean_census <- function(df, census_vars, aggregate_categories) {

	df %>%
		dplyr::rename(fips = GEOID, county = NAME) %>%
		dplyr::filter(fips %in% unique(outcome$fips)) %>%
		dplyr::left_join(aggregate_categories, by = "variable") %>%
		dplyr::mutate(variable = dplyr::coalesce(category, variable)) %>%
		dplyr::group_by(fips, county, variable) %>%
		dplyr::summarize(estimate = sum(estimate)) %>%
		dplyr::ungroup() %>%
		dplyr::left_join(census_vars, by = "variable") %>%
		dplyr::mutate(variable_name = dplyr::coalesce(variable_name, variable))
}

#' @title Turn counts into rates
#' @param df: Data
#' @param population_var: Census variable for population
#' @param leave_as_is: Vector of variables to leave without calculating rates
normalize_population <- function(df,
																 population_var = "B01001_001",
																 leave_as_is = c("B19083_001", "B19013_001")) {

	population <- df %>%
		dplyr::filter(variable == population_var) %>%
		dplyr::select(fips, population = estimate)

	df %>%
		dplyr::inner_join(population) %>%
		dplyr::arrange(fips, variable) %>%
		dplyr::mutate(estimate = ifelse(variable %in% leave_as_is, estimate, estimate/population*100000))
}

#' @title Main census data management
#' @param variables: List of census variables
#' @param fn_raw: File location at which to store initial census download
#' @param fn_proc: File location at which to store cleaned census data
#' @param update: Boolean, whether to update or pull from file
get_census <- function(variables = config$sources$census$variables,
											 fn_raw = config$data$census$raw,
											 fn_proc = config$data$census$proc,
											 update = F) {

	if (!file.exists(fn_raw) | update) {

		census_vars <- tidycensus::load_variables(year = "2019", dataset = "acs5", cache = T) %>%
			dplyr::mutate(variable_name = paste(concept, label, sep = ": ")) %>%
			dplyr::select(variable = name, variable_name)

		aggregate_categories <- tibble::tribble(
			~ "category", ~ "variable",
			"insured", "B27022_004",
			"uninsured", "B27022_005",
			"insured", "B27022_007",
			"uninsured", "B27022_008",
			"insured", "B27022_011",
			"uninsured", "B27022_012",
			"insured", "B27022_014",
			"uninsured", "B27022_015"
		)

		df <- download_census(variables = variables, fn_raw = fn_raw) %>%
			clean_census(census_vars = census_vars, aggregate_categories = aggregate_categories) %>%
			normalize_population()

		readr::write_csv(x = df,
										 file = fn_proc)

	} else {
		df <- readr::read_csv(fn_proc)
	}

	return(df)
}
