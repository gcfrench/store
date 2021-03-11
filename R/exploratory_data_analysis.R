#' display_variable_summary
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the basic statistics of the variables within a data frame into an output
#' directory. This includes a plots of the prevalence of missing values and frequency
#' of category levels and a table image containing a summary count of the number of rows,
#' missing values, unique values and zero values for each variable
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, data frame to summarise
#'
#' @return returns dataset invisibly
#' @export
display_variable_summary <- function(.dataset) {

  # export plot as image
  export_plot <- function(plot, plot_name,
                          figure_width = 6, figure_height = 6) {
    ggplot2::ggsave(filename = stringr::str_glue("output/{plot_name}.png"),
                    plot = plot,
                    type = "cairo-png",
                    width = figure_width,
                    height = figure_height,
                    units = "in",
                    dpi = 72)
    invisible(plot)
  }

  # check for types
  check_numeric <- any(c("numeric") %in% (dlookr::diagnose(.dataset)$types))
  check_nominal <- any(c("factor", "character") %in% (dlookr::diagnose(.dataset)$types))

  # sort column names
  .dataset <- .dataset %>%
    dplyr::select(sort(names(.)))

  # variable summary table --------------------------------------------------

  ## numeric variable summary table
  if(check_numeric) {
    numeric_table <- dlookr::diagnose(.dataset) %>%
      dplyr::inner_join(dlookr::diagnose_numeric(.dataset)) %>%
      dplyr::inner_join(dlookr::describe(.dataset) %>%
                          dplyr::rename(variables = variable)) %>%
      dplyr::select(variables, types, n, contains("count"), zero)
  }

  ## nominal variable statistics table
  if(check_nominal) {
    nominal_table <-dlookr::diagnose(.dataset) %>%
      dplyr::inner_join(dlookr::diagnose_category(.dataset)) %>%
      dplyr::rename(n = N) %>%
      dplyr::select(variables, types, n, contains("count")) %>%
      dplyr::distinct()
  }

  ## combined numeric and nominal tables
  if(check_numeric & check_nominal) {
    summary_table <- numeric_table %>%
      dplyr::bind_rows(nominal_table)
  } else if(check_numeric) {
    summary_table <- numeric_table
  } else {
    summary_table<- nominal_table
  }

  ## export variable statistics table
  gridExtra::tableGrob(summary_table, rows = NULL,
                       theme = gridExtra::ttheme_default(base_size = 16)) %>%
    export_plot("summary_table", figure_width = 10)

  # variable missing data plot
  inspectdf::inspect_na(.dataset) %>%
    inspectdf::show_plot() %>%
    export_plot("missing_data")

  # variable category plot
  if(check_nominal) {
    inspectdf::inspect_cat(.dataset) %>%
      inspectdf::show_plot() %>%
      export_plot("category_data", figure_width = 10)
  }

  invisible(.dataset)
}

#' display_variable_distribution
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Displays the distribution of the numeric variables within a data frame, including
#' a histogram for each variable and table of summary statistics. These statistics
#' include the range, quartiles, mean, medium, standard deviation, standard error of
#' the mean, level of skewness, kurtosis and normality
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, data frame used to display the variable statistics
#' @param height_1 integer, integer between 0-1, determining height of first column
#' @param height_2 integer, integer between 0-1, determining height of second column
#'
#' @return a patchwork object including one plot and one table
#' @export
display_variable_distribution <- function(.dataset, height_1 = 1, height_2 = 0) {

  # check for types
  check_numeric <- any(c("numeric") %in% (dlookr::diagnose(.dataset)$types))
  check_nominal <- any(c("factor", "character") %in% (dlookr::diagnose(.dataset)$types))

  # exit function if no numeric types
  if(!check_numeric) {
    return(NULL)
  }

  # sort column names
  .dataset <- .dataset %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(sort(names(.)))

  # variable distribution table
  t1 <- dlookr::diagnose_numeric(.dataset) %>%
    dplyr::inner_join(dlookr::describe(.dataset)
                      %>% dplyr::rename(variables = variable)) %>%
    dplyr::inner_join(dlookr::normality(.dataset) %>%
                        dplyr::rename(variables = vars, normality = statistic)) %>%
    dplyr::select(variables:max, sd:kurtosis, normality, p_value)
  t1 <- gridExtra::tableGrob(t1, rows = NULL, theme = gridExtra::ttheme_minimal())

  # variable distribution plot
  p1 <- inspectdf::inspect_num(.dataset) %>%
    inspectdf::show_plot()

  # display plot and table
  patchwork::wrap_plots(p1 / patchwork::wrap_elements(t1),
                        heights = c(height_1, height_2))
}

#' display_variable_outliers
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Displays the variable outliers within a data frame with a boxplot, and
#' summary statistics including count outliers and mean of each variable
#' with outliers included and excluded
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, data frame used to display the variable statistics
#' @param width_1 integer, integer between 0-1, determining width of first column
#' @param width_2 integer, integer between 0-1, determining width of second column
#'
#' @return a patchwork object including one plot and one table
#' @export
display_variable_outliers <- function(.dataset, width_1 = 0.6, width_2 = 0.4) {

  # check for types
  check_numeric <- any(c("numeric") %in% (dlookr::diagnose(.dataset)$types))
  check_nominal <- any(c("factor", "character") %in% (dlookr::diagnose(.dataset)$types))

  # exit function if no numeric types
  if(!check_numeric) {
    return(NULL)
  }

  # sort column names
  .dataset <- .dataset %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(sort(names(.)))

  # variable outliers table
  t1 <- dlookr::diagnose_outlier(.dataset) %>%
    dplyr::rename(outliers_count = outliers_cnt,
                  with_outliers_mean = with_mean,
                  without_outliers_mean = without_mean) %>%
    dplyr::select(-outliers_ratio, -outliers_mean)
  t1 <- gridExtra::tableGrob(t1, rows = NULL, theme = gridExtra::ttheme_minimal())

  # variable outliers plot
  p1 <- .dataset %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(ggplot2::vars(name), scale = "free") +
    ggplot2::guides(fill = FALSE) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  # display plot and table
  patchwork::wrap_plots(p1, patchwork::wrap_elements(t1), widths = c(width_1, width_2))
}

#' display_variable_correlation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Displays a plot of the correlation matrix for each variable, showing the
#' correlation values between each variable combination
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, data frame used to display the variable statistics
#'
#' @return a corrplot object of the correlation matrix plot
#' @export
display_variable_correlation <- function(.dataset) {

  # check for types
  check_numeric <- any(c("numeric", "integer") %in% (dlookr::diagnose(.dataset)$types))
  check_nominal <- any(c("factor", "character") %in% (dlookr::diagnose(.dataset)$types))

  # exit function if no numeric types
  if(!check_numeric) {
    return(check_numeric)
  }

  # sort column names
  .dataset <- .dataset %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(sort(names(.)))

  # variable correlation plot
  p1 <- .dataset %>%
    cor(use = "complete.obs", method = "pearson") %>%
    corrplot::corrplot(method = "number", number.digits = 3,
                       type = "upper", mar = c(0, 0, 2, 0),
                       title = "Correlation between numeric variables")

  # display plot
  p1

  # do not return correlation matrix
  invisible()
}
