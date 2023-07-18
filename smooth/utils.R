# packages ----------------------------------------------------------------
library(ggplot2)
library(rlang)

# plots -------------------------------------------------------------------
smooth_plot <- function(df, x_var, y_var, col_var, method) {
  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[col_var]]
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = method)
}

point_plot <- function(df, x_var, y_var, col_var) {
  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[col_var]]
    )
  ) +
    ggplot2::geom_point()
}
