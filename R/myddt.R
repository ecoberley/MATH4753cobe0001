#' myddt
#'
#' @param df the DDT dataframe
#' @param SPECIES the species to subset df with
#'
#' @return Named list of dataframe before subsetting, dataframe after subsetting, and relative frequency of RIVER values before subsetting
#' @export
#'
#' @examples
#' \dontrun{myddt(ddt, "CCATFISH")}
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
