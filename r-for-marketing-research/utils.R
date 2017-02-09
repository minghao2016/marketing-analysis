convert_to_factor <- function(df) {
  df[] <- lapply(df[], function(x) {
    if(class(x) == "character") as.factor(x) else x
  })
  df
}

reset_rownames <- function(df, col="rowname") {
  stopifnot(is.data.frame(df))
  col <- as.character(substitute(col))
  reset_rownames_(df, col)
}

reset_rownames_ <- function(df, col="rowname") {
  stopifnot(is.data.frame(df))
  nm <- data.frame(df)[, col]
  df <- df[, !(colnames(df) %in% col)]
  rownames(df) <- nm
  df
}