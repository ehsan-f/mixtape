bin <- function (x, y, data, type = "a", q = 0.2, cut = NULL, silent = F) 
{
  #-- Packages
  require(smbinning)
  
  #-- Dataframe
  data <- as.data.frame(data)
  
  #-- Binning
  if (type == "a") {
    sbin <- smbinning(df = data, y = y, x = x, p = 0.05)
  }
  else if (type == "f") {
    data[, x] <- as.factor(data[, x])
    sbin <- smbinning.factor(df = data, y = y, x = x, maxcat = 200)
  }
  else if (type == "q") {
    cutoff <- quantile(data[, x], probs = seq(0, 1, q), na.rm = T)
    cutoff <- as.vector(cutoff)
    cutoff <- cutoff[2:(length(cutoff) - 1)]
    sbin <- smbinning.custom(df = data, y = y, x = x, cuts = cutoff)
  }
  else if (type == "c") {
    if (is.null(cut) == TRUE) {
      sbin <- smbinning(df = data, y = y, x = x, p = 0.05)
    }
    else {
      sbin <- smbinning.custom(df = data, y = y, x = x, 
                               cuts = cut)
    }
  }
  
  #-- Output
  if (silent == F) {
    print(sbin$ivtable)
    smbinning.plot(sbin, option = "WoE", sub = x)
  } 
  
  list(info = sbin, type = type) %>% invisible()
}