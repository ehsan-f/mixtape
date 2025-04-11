#' Create binning for a predictor variable
#'
#' @description
#' Creates binning for a predictor variable with respect to a response variable.
#' Supports automatic, factor, quantile, and custom binning methods.
#' Can also produce Weight of Evidence plots.
#'
#' @param x Name of the predictor variable
#' @param y Name of the response variable
#' @param data Data frame containing the variables
#' @param type Binning type: 'a' (automatic), 'f' (factor), 'q' (quantile), 'c' (custom) (default: 'a')
#' @param q Quantile step for quantile binning (default: 0.2)
#' @param cut Vector of cut points for custom binning (optional)
#' @param silent Whether to suppress output (default: FALSE)
#' @param custom_plot Whether to use custom plot styling (default: TRUE)
#' @param custom_colour_scale_start Start color for custom plot gradient (default: '#5720FF')
#' @param custom_colour_scale_end End color for custom plot gradient (default: 'grey')
#'
#' @export
bin <- function (x, y, data, type = "a", q = 0.2, cut = NULL, silent = F,
                 custom_plot = T, custom_colour_scale_start = '#5720FF', custom_colour_scale_end = 'grey')
{
  #-- Packages
  library(smbinning)

  #-- Dataframe
  data <- as.data.frame(data)

  #-- Binning
  if (type == "a") {
    sbin <- smbinning(df = data, y = y, x = x, p = 0.05)
  } else if (type == "f") {
    data[, x] <- as.factor(data[, x])
    sbin <- smbinning.factor(df = data, y = y, x = x, maxcat = 200)
  } else if (type == "q") {
    cutoff <- quantile(data[, x], probs = seq(0, 1, q), na.rm = T)
    cutoff <- as.vector(cutoff)
    cutoff <- cutoff[2:(length(cutoff) - 1)]
    sbin <- smbinning.custom(df = data, y = y, x = x, cuts = cutoff)
  } else if (type == "c") {
    if (is.null(cut) == TRUE) {
      sbin <- smbinning(df = data, y = y, x = x, p = 0.05)
    } else {
      sbin <- smbinning.custom(df = data, y = y, x = x,
                               cuts = cut)
    }
  }

  #-- Output
  if (silent == F) {
    #- Table
    print(sbin$ivtable)

    #- Plot
    if (custom_plot == T) {
      sbin_gg <- sbin$ivtable %>%
        #-- Fix table
        as_tibble() %>%
        mutate(
          exclude = if_else(Cutpoint == 'Total' | CntRec == 0, 1, 0)
        ) %>%
        filter(exclude == 0) %>%
        mutate(
          WoE = round(WoE, digits = 2)
        ) %>%
        #-- Plot
        ggplot(aes(x = Cutpoint, y = WoE, label = WoE, fill = Cutpoint)) +

        geom_bar(stat = 'identity', color = 'black') +
        geom_label(color = 'black', fill = 'white', show.legend = F, size = 4.5, label.size = 0.5) +

        #- Colour scale
        scale_fill_manual(values = colorRampPalette(c(custom_colour_scale_start, custom_colour_scale_end))(n = (nrow(sbin$ivtable) - 1))) +

        #- Labels
        labs(title = 'Weight of Evidence', subtitle = paste0(sbin$x, ' (IV = ', sbin$iv, ')')) +
        xlab(label = '') +

        #- Theme
        theme_minimal(base_size = 14) +
        theme(
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, color = custom_colour_scale_start),
          axis.title = element_text(face = 'bold'),

          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )

      print(sbin_gg)
    } else {
      smbinning.plot(sbin, option = "WoE", sub = x)
    }
  }

  list(info = sbin, type = type) %>% invisible()
}
