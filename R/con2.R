#' Grouped Continuous Variable
#'
#' This function allows you to summarize a continuous variable by group for tables
#' @param x Grouping variable
#' @param y Variable to summarize per group
#' @param d Digits to print. Defaults to 1
#' @param nam Lable of characteristic column. Defaults to ""
#' @param prmr Include median and range? Defaults to FALSE.
#' @keywords table1
#' @export
#' @examples
#' con2()

con2 <- function(x, y, d = 1, nam = "", prmr = TRUE) {
  if (prmr == TRUE) {
    rbind(
      matrix("", ncol = length(levels(as.factor(x))), dimnames = list(nam)),
      matrix(
        c(by(y, x, function(j) {
          sprintf(paste0("%1.", d, "f (%1.", d, "f) [%1.0f]"),
                  mean(j, na.rm = TRUE),
                  sd(j, na.rm = TRUE),
                  sum(is.na(j)))
        }),
        by(y, x, function (k) {
          sprintf(paste0("%1.", d, "f [%1.", d, "f, %1.", d, "f]"),
                  median(k, na.rm = T),
                  min(k, na.rm = T),
                  max(k, na.rm = T))
        })),
        nrow = 2, byrow = T, dimnames = list(c(
          "&nbsp;&nbsp;Mean (SD) [Missing]",
          "&nbsp;&nbsp;Median [Range]"
        ), levels(as.factor(x)))))
  } else {
    rbind(
      matrix("", ncol = length(levels(as.factor(x))), dimnames = list(nam)),
      matrix(
        c(by(y, x, function(j) {
          sprintf(paste0("%1.", d, "f (%1.", d, "f) [%1.0f]"),
                  mean(j, na.rm = T),
                  sd(j, na.rm = T),
                  sum(is.na(j)))
        })),
        nrow = 1, byrow = T, dimnames = list(c(
          "&nbsp;&nbsp;Mean (SD) [Missing]"
        ), levels(as.factor(x)))))
  }
}

