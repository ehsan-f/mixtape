mix_load_infix <- function() {
  
  #----- % functions
  `%>=<%` <<- function(x, range) {
    x >= range[1] & x <= range[2]
  }
  
  `%>= <%` <<- function(x, range) {
    x >= range[1] & x < range[2]
  }
  
  `%> =<%` <<- function(x, range) {
    x > range[1] & x <= range[2]
  }
  
  `%> <%` <<- function(x, range) {
    x > range[1] & x < range[2]
  }
  
  `%limit%` <<- function(x, range) {
    ifelse(x <= range[1], range[1],
           ifelse(x >= range[2], range[2],
                  x))
  }
  
  `%bracket_max%` <<- function(x, range) {
    range <- sort(range)
    for (i in 1:length(range)) {
      if (x <= range[i]) {
        return(range[i])
      }
    }
  }
  
  `%bracket_min%` <<- function(x, range) {
    range <- sort(range, decreasing = T)
    for (i in 1:length(range)) {
      if (x >= range[i]) {
        return(range[i])
      }
    }
  }
  
}
