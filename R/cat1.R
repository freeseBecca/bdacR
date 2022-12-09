#' One categorical variable
#'
#' This function allows you to get variable summaries for a categorical variable for tables.
#' @param y categorical variable to summarize
#' @param l Column label. Defaults to "OVerall"
#' @param u Include row for missing values? Defaults to TRUE
#' @param ulab Label for missing value row. Defaults to "Missing"
#' @param nam Variable label. Defaults to ""
#' @keywords table1
#' @export
#' @examples
#' cat1()

cat1 <- function(y, l = "Overall", u = TRUE, ulab = "&nbsp;Missing", nam = "") {
  if (u == T) {
    matrix(c("", sprintf("%1.0f (%1.1f)",
                         table(y),
                         100 * prop.table(table(y))),
             sum(is.na(y))),
           ncol = 1,
           dimnames = list(
             c(paste0(nam, ", N (%)"),
               paste0("&nbsp;&nbsp;", c(rownames(table(y)))), ulab), l))
  }
  else if (u == F) {
    matrix(c("", sprintf("%1.0f (%1.1f)",
                         table(y),
                         100 * prop.table(table(y)))),
           ncol = 1,
           dimnames = list(
             c(paste0(nam, ", N (%)"),
               paste0("&nbsp;&nbsp;", rownames(table(y)))), l))
  }
}
