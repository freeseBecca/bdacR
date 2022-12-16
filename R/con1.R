#' One Continuous Variable
#'
#' This function allows you to summarize a continuous variable for tables
#' @param y Variable to summarize
#' @param l Label for summary column. Defaults to "Overall"
#' @param prmr Include median and range? Defaults to FALSE
#' @param nam Label of summary variable
#' @keywords table1
#' @export
#' @examples
#' con1()

con1 <- function (y, l = "Overall", prmr = FALSE, nam = "")
{
  if (prmr == TRUE) {
    matrix(c("", sprintf("%1.1f (%1.2f) [%1.0f]",
                         mean(y, na.rm = T), sd(y, na.rm = T), sum(is.na(y))),
             sprintf("%1.1f [%1.1f, %1.1f]", median(y, na.rm = T),
                     min(y, na.rm = T), max(y, na.rm = T))), nrow = 3,
           byrow = T, dimnames = list(c(nam, "Mean (SD) [Missing]",
                                        "Median [Range]"), l))
  }
  else {
    matrix(c("", sprintf("%1.2f (%1.2f) [%1.0f]",
                         mean(y, na.rm = T), sd(y, na.rm = T), sum(is.na(y)))),
           nrow = 2, byrow = T, dimnames = list(c(nam, "Mean (SD) [Missing]"),
                                                l))
  }
}

cat1 <- function (y, l = "Overall", u = TRUE, ulab = "Missing",
                  nam = "")
{
  if (u == T) {
    matrix(c("", sprintf("%1.0f (%1.1f)", table(y),
                         100 * prop.table(table(y))), sum(is.na(y))), ncol = 1,
           dimnames = list(c(paste0(nam, ", N (%)"), paste0(c(rownames(table(y)))), ulab), l))
  }
  else if (u == F) {
    matrix(c("", sprintf("%1.0f (%1.1f)", table(y),
                         100 * prop.table(table(y)))), ncol = 1, dimnames = list(c(paste0(nam,
                                                                                          ", N (%)"), paste0(rownames(table(y)))),
                                                                                 l))
  }
}

