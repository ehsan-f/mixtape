#' @export
auto_bin <- function(sbin, df = ds) {

  #-- Extract bin info
  cuts <- sbin$info$cuts
  woe_all <- sbin$info$ivtable$WoE
  x <- sbin$info$x
  type <- sbin$type

  if (!is.na(woe_all[ (length(woe_all) - 1) ])) {
    woe_miss <- woe_all[(length(woe_all)-1)]
  }

  woe <- woe_all[1:(length(woe_all) - 2)]
  #================================================================================#
  #-- Apply binnings
  df$var <- df[, x]
  df$var_CC <- 0

  if (type != "f") {

    if (length(cuts) == 1) {
      df$var_CC[df$var <= cuts[1]] <- woe[1]
      df$var_CC[df$var >  cuts[1]] <- woe[2]
    } else {

      df$var_CC[df$var <= cuts[1]] <- woe[1]

      for (i in 2:(length(woe)-1)) {
        df$var_CC[df$var > cuts[i-1] & df$var <= cuts[i]] <- woe[i]
      }
      df$var_CC[df$var > cuts[length(cuts)]] <- woe[length(woe)]

    }


  }

  else if (type == "f") {
    for (i in 1:length(woe)) {
      df$var_CC[df$var == cuts[i]] <- woe[i]
    }
  }

  #-- Assign missing value WoE
  if (!is.na(woe_all[ (length(woe_all) - 1) ])) {
    df$var_CC[is.na(df$var)] <- woe_miss
  }

  #-- Print
  print(table(df$var_CC))

  #-- Output
  df$var_CC <- as.numeric(df$var_CC)

}
