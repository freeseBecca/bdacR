#' A table function for logistic regression models
#'
#' This function allows you to make nice summary tables for logistic regression models.
#' @param m logistic model object
#' @param d digits to round table. Defaults to 4
#' @param alpha confidence level. Defaults to 0.95
#' @export
#' @examples
#' make_logistic_data()

make_logistic_data <- function(m, d = 4, alpha = 0.95){
  m_data <- as.data.table(exp(cbind(Estimate = coef(m), confint(m, level = alpha))))
  m_data[, p_value := coef(summary(m))[,4]]
  cols <- names(m_data)
  m_data[,(cols) := round(.SD, d), .SDcols=cols]
  m_data[, `95% CI` := paste0("(", `2.5 %`, ", ", `97.5 %`, ")")]
  m_data[, Variable := names(coef(m))]

  m_data[, .(Variable, Estimate, p_value, `95% CI`)]
}
