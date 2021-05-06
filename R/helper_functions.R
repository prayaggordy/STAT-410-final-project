#' @title Print a fitted object from a step function as LaTeX
print_latex_equation <- function(fit) {

	x_vars <- fit$anova$Step %>%
		gsub(pattern = "\\+ ", replacement = "", x = .) %>%
		gsub(pattern = "\\_", replacement = " ", x = .) %>%
		gsub(pattern = "\\:", replacement = "*", x = .)

	y_var <- fit$call$formula[[2]] %>%
		as.character() %>%
		gsub(pattern = "\\_", replacement = " ", x = .)

	paste("$$", y_var, "=", "\\boldsymbol{\\beta}'", "\\begin{bmatrix}", paste0(x_vars[-1], collapse = "\\\\"), "\\end{bmatrix}", "+", "\\boldsymbol{\\epsilon}", "$$")
}