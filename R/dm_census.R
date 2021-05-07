library(magrittr)

config <- yaml::read_yaml("./config.yaml")

#' @title General function for getting sequential census variables
#' @param l: List in config
#' @param init: Starting part of census variables
build_long_vars <- function(l, init) {
	lapply(l,
				 function(sex) {
				 	paste0(
				 		init,
				 		stringr::str_pad(sex$start:sex$end, width = 2, side = "left", pad = "0")
				 	)
				 }) %>%
		unlist() %>%
		unname()
}

#' @title Get variables and groups for downloading age data
build_age_vars <- function() {
	build_long_vars(l = config$census_long$age$sex, init = config$census_long$age$open)
}

build_insurance_vars <- function(census_vars) {
	build_long_vars(l = config$census_long$insurance$sex, init = config$census_long$insurance$open) %>%
		tibble::as_tibble() %>%
		dplyr::rename(variable = value) %>%
		dplyr::inner_join(census_vars, by = "variable") %>%
		dplyr::filter(grepl(pattern = "health insurance coverage", x = variable_name)) %>%
		dplyr::pull(variable)
}

build_aggregate_categories <- function(census_vars) {
	age_groups <- tibble::tibble(variable = build_age_vars(),
															 category = rep(c(rep("under_18", 4),
															 								 rep("between_18_44", 8),
															 								 rep("between_45_65", 5),
															 								 rep("above_65", 6)), 2))

	insurance_groups <- tibble::tibble(variable = build_insurance_vars(census_vars = census_vars)) %>%
		dplyr::inner_join(census_vars, by = "variable") %>%
		tidyr::separate(col = variable_name, into = c("age_part", "category"), sep = "years:!!|over:!!") %>%
		dplyr::select(variable, category) %>%
		dplyr::mutate(category = ifelse(category == "With health insurance coverage", "insured", "uninsured"))

	return(dplyr::bind_rows(age_groups, insurance_groups))
}

#' @title Download the raw census data
#' @param variables: The census variables to download
#' @param fn_raw: File location at which to store raw data
download_census <- function(variables, census_vars, fn_raw) {

	df <- tidycensus::get_acs(geography = "county", variables = variables) %>%
		dplyr::bind_rows(tidycensus::get_acs(geography = "county", variables = build_age_vars())) %>%
		dplyr::bind_rows(tidycensus::get_acs(geography = "county", variables = build_insurance_vars(census_vars = census_vars)))

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
																 leave_as_is = c("B01001_001", "B19083_001", "B19013_001")) {

	population <- df %>%
		dplyr::filter(variable == population_var) %>%
		dplyr::select(fips, population = estimate)

	df %>%
		dplyr::inner_join(population) %>%
		dplyr::arrange(fips, variable) %>%
		dplyr::mutate(estimate = ifelse(variable %in% leave_as_is, estimate, estimate/population*config$rate)) %>%
		dplyr::select(-population)
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

		aggregate_categories <- build_aggregate_categories(census_vars = census_vars)

		df <- download_census(variables = variables, census_vars = census_vars, fn_raw = fn_raw) %>%
			clean_census(census_vars = census_vars, aggregate_categories = aggregate_categories) %>%
			normalize_population() %>%
			dplyr::mutate(variable_name_short = dplyr::case_when(
				variable == "B01001_001" ~ "population",
				variable == "B01001_002" ~ "male",
				variable == "B01001_026" ~ "female",
				variable == "B01001B_001" ~ "black",
				variable == "B01001D_001" ~ "asian",
				variable == "B01001H_001" ~ "white",
				variable == "B01001I_001" ~ "hispanic",
				variable == "B19013_001" ~ "income",
				variable == "B19083_001" ~ "gini",
				variable == "B22002_002" ~ "food_stamp",
				variable == "B23006_002" ~ "less_HS",
				variable == "B23006_009" ~ "complete_HS",
				variable == "B23006_016" ~ "some_college",
				variable == "B23006_023" ~ "college",
				TRUE ~ variable
			))

		readr::write_csv(x = df,
										 file = fn_proc)

	} else {
		df <- readr::read_csv(fn_proc)
	}

	return(df)
}
