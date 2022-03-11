#' Title
#'
#' @param df
#' @param SPECIES
#'
#' @return
#' @export
#'
#' @examples
myddt = function(df = ddt, SPECIES = "CCATFISH") {
  library(ggplot2)
  library(dplyr)

  ddt <- df
  df <- df %>% filter(SPECIES == {{SPECIES}})
  g <- ggplot(df, aes(x = WEIGHT, y = LENGTH)) +
    geom_point(aes(col = RIVER)) +
    geom_smooth(formula = y~x + I(x^2), method = "lm") +
    labs(title = "Evan Coberley")
  print(g)

  path <- sprintf("LvsWfor%s.csv", SPECIES)
  write.csv(df, path)

  nlist <- list(ddt, df, table(ddt$RIVER)/length(ddt$RIVER))
  names(nlist) <- c("Raw data", "Subsetted data", "Relative Frequency of RIVER, raw data")
  print(nlist)

}
