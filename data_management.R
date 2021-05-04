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
}

get_census <- function(variables = config$sources$census$variables,
											 update = F,
											 fn_raw = config$data$census$raw,
											 fn_proc = config$data$census$proc) {

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

		df <- tidycensus::get_acs(geography = "county", variables = variables)
		readr::write_csv(x = df,
										 file = fn_raw)

		df <- df %>%
			dplyr::rename(fips = GEOID, county = NAME) %>%
			dplyr::filter(fips %in% unique(outcome$fips)) %>%
			dplyr::left_join(aggregate_categories, by = "variable") %>%
			dplyr::mutate(variable = dplyr::coalesce(category, variable)) %>%
			dplyr::group_by(fips, county, variable) %>%
			dplyr::summarize(estimate = sum(estimate)) %>%
			dplyr::ungroup() %>%
			dplyr::left_join(census_vars, by = "variable") %>%
			dplyr::mutate(variable_name = dplyr::coalesce(variable_name, variable))

		population <- df %>%
			dplyr::filter(variable == "B01001_001") %>%
			dplyr::select(fips, population = estimate)

		df <- df %>%
			dplyr::inner_join(population) %>%
			dplyr::arrange(fips, variable) %>%
			dplyr::mutate(estimate = ifelse(variable == "B19083_001", estimate, estimate/population))

		readr::write_csv(x = df,
										 file = fn_proc)

	} else {
		df <- readr::read_csv(fn_proc)
	}

	return(df)
}

cen <- get_census(update = T)
