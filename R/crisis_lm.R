#' Title
#'
#' @param data1 data
#'
#' @return list
#' @import stats
#' @import base
#' @import MASS
#' @import graphics
#' @export
#'
#' @examples
crisis_lm <- function(data1){
  a <- data1$systemic_crisis
  b <- data1$exch_usd
  c <- data1$domestic_debt_in_default
  d <- data1$sovereign_external_debt_default
  e <- data1$gdp_weighted_default
  f <- data1$inflation_annual_cpi
  g <- data1$independence
  h <- data1$currency_crises
  i <- data1$inflation_crises
  j <- data1$banking_crisis

  # matrice de scatter plot
  scartter <- graphics::pairs(data.frame(a, b, c, d, e, f, g, h, i, j), pch = 1,
                    lower.panel=panel.smooth,
                    col = "blue", main = "Matrice de Scatter plot")
  # Regression lineaire
  ml <- stats::lm(a~b+c+d+e+f+g+h+i+j,data1)

  # test sur les paramètres
  resum <-base::summary(ml)
  # analyse des variances
  anov <- stats::anova(ml)

  # Sélection des variables : pas à pas de façon automatique :
  selec <-  stepAIC(lm(a~1, data1),a~b+c+d+e+f+g+h+i+j, direction="both")

  sortie <- base::list(scatterplot = scartter, regression_lineaire = ml, resume = resum,
              anova = anov, selection_variable = selec)
  return(sortie)

}
