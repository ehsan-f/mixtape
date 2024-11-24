#' @export
mix_theme <- function(x_axis_angle = NULL, gg_theme = theme_light()) {
  #-- Packages
  library(ggplot2)
  #-- Theme
  x_angle <- NULL
  if (!is.null(x_axis_angle)) {
    x_angle <- theme(axis.text.x = element_text(angle = x_axis_angle))
  }

  output <- gg_theme +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = mix_palette$blue,
                                   face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      strip.text.x = element_text(face = "bold"),

      legend.background = element_rect(linetype = "solid",
                                       color = mix_palette$light_grey)
    ) +
    x_angle

}
