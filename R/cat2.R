#' Two Categorical Variables
#'
#' This function allows you to summarize a categorical variable by a second grouping variable for tables.
#' @param x Grouping variable
#' @param y Variable to summarize per group
#' @param u Include row for missing values? Defaults to TRUE
#' @param ulab Label for missing values
#' @param r Values, either 1 default or 2 - columns sum to 100 %
#' @param nam Label of variable summarized. Defaults to ""
#' @keywords
#' @export
#' @examples
#' cat2()

cat2 <- function(x, y, u = TRUE, ulab = "Missing", r = 1, nam = "") {
  if (u == T) {
    cbind(
      rbind(matrix("", ncol = ncol(table(y, x)), dimnames = list(nam)),
            matrix(
              sprintf(
                "%1.0f (%1.1f)", table(y, x), 100 * prop.table(table(y, x), r)
              ), nrow = nrow(table(y, x)), ncol = ncol(table(y, x)), dimnames = list(paste("&nbsp;&nbsp;", rownames(
                table(y, x)
              ), sep = ""), colnames(table(y, x)))
            ),
            matrix(table(x)-table(is.na(y), x)[1,], nrow = 1, dimnames = list("Missing")))
    )
  }
  else if (u == F) {
    cbind(
      rbind(matrix("", ncol = ncol(table(y, x)), dimnames = list(nam)),
            matrix(
              sprintf(
                "%1.0f (%1.1f)", table(y, x), 100 * prop.table(table(y, x), r)
              ), nrow = nrow(table(y, x)), ncol = ncol(table(y, x)), dimnames = list(paste("&nbsp;&nbsp;", rownames(
                table(y, x)
              ), sep = ""), colnames(table(y, x)))))
    ) }
}
