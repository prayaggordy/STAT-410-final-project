#' @title Print a fitted object from a step function as LaTeX
print_latex_equation <- function(fit) {

	x_vars <- fit$call$formula[[3]] %>%
		as.character() %>%
		paste(collapse = " + ") %>%
		gsub(pattern = "\\_", replacement = " ", x = .) %>%
		gsub(pattern = "\\:", replacement = "*", x = .) %>%
		stringr::str_split(pattern = " \\+ ")

	y_var <- fit$call$formula[[2]] %>%
		as.character() %>%
		gsub(pattern = "\\_", replacement = " ", x = .)

	paste("$$", y_var, "=", "\\boldsymbol{\\beta}'", "\\begin{bmatrix}", paste0(x_vars[[1]][-1], collapse = "\\\\"), "\\end{bmatrix}", "+", "\\boldsymbol{\\epsilon}", "$$")
}