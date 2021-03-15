#' display_variable_summary
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the basic statistics of the variables within a data frame into an output
#' directory. This includes a plots of the prevalence of missing values and frequency
#' of category levels and a table image containing variable's type summary count of
#' the number of rows, missing values, unique values and zero values for each variable
#'
#' @seealso
#' This is an example of exploratory data analsyis using the [dlookr](https://github.com/choonghyunryu/dlookr)
#' and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, dataset for exploratory data analysis
#'
#' @return dataset returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(palmerpenguins)
#'   library(here)
#'   library(fs)
#'  })
#' })
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir.create("output")}
#'
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' display_variable_summary(penguins_raw)
#' }
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
    export_plot("01-summary_table", figure_width = 10)

  # variable missing data plot
  inspectdf::inspect_na(.dataset) %>%
    inspectdf::show_plot() %>%
    export_plot("02-missing_data")

  # variable category plot
  if(check_nominal) {
    inspectdf::inspect_cat(.dataset) %>%
      inspectdf::show_plot() %>%
      export_plot("03-category_data", figure_width = 10)
  }

  invisible(.dataset)
}

#' display_variable_outliers
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the variable outliers within a data frame with a boxplot, and
#' summary statistics including count outliers and mean of each variable
#' with outliers included and excluded
#'
#' @seealso
#' This is an example of exploratory data analsyis using the [dlookr](https://github.com/choonghyunryu/dlookr)
#' and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, dataset for exploratory data analysis
#'
#' @return dataset returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(palmerpenguins)
#'   library(here)
#'   library(fs)
#'  })
#' })
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir.create("output")}
#'
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' display_variable_outliers(penguins_raw)
#' }
display_variable_outliers <- function(.dataset) {

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

  # exit function if no numeric types
  if(!check_numeric) {
    return(NULL)
  }

  # sort column names
  .dataset <- .dataset %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(sort(names(.)))

  # variable outliers table
  outliers_table <- dlookr::diagnose_outlier(.dataset) %>%
    dplyr::rename(outliers_count = outliers_cnt,
                  with_outliers_mean = with_mean,
                  without_outliers_mean = without_mean) %>%
    dplyr::select(-outliers_ratio, -outliers_mean)

  # export outliers table
  gridExtra::tableGrob(outliers_table, rows = NULL, theme = gridExtra::ttheme_default(base_size = 16)) %>%
    export_plot("04-outliers_table", figure_width = 12)

  # variable outliers plot
  variable_outliers <- .dataset %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(ggplot2::vars(name), scale = "free") +
    ggplot2::guides(fill = FALSE) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  # export variable outliers plot
  variable_outliers %>%
    export_plot("05-variable_outliers")

  invisible(.dataset)
}

#' display_variable_distribution
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the distribution of the numeric variables within a data frame, including
#' a histogram for each variable and table of summary statistics. These statistics
#' include the range, quartiles, mean, medium, standard deviation, standard error of
#' the mean, level of skewness, kurtosis and normality
#'
#' @seealso
#' This is an example of exploratory data analsyis using the [dlookr](https://github.com/choonghyunryu/dlookr)
#' and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, dataset for exploratory data analysis
#'
#' @return dataset returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(palmerpenguins)
#'   library(here)
#'   library(fs)
#'  })
#' })
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir.create("output")}
#'
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' display_variable_distribution(penguins_raw)
#' }
display_variable_distribution <- function(.dataset) {

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

  # exit function if no numeric types
  if(!check_numeric) {
    return(NULL)
  }

  # sort column names
  .dataset <- .dataset %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(sort(names(.)))

  # variable distribution table
  distribution_table <- dlookr::diagnose_numeric(.dataset) %>%
    dplyr::inner_join(dlookr::describe(.dataset)
                      %>% dplyr::rename(variables = variable)) %>%
    dplyr::inner_join(dlookr::normality(.dataset) %>%
                        dplyr::rename(variables = vars, normality = statistic)) %>%
    dplyr::select(variables:max, sd:kurtosis, normality, p_value)

  # export variable distribution table
  gridExtra::tableGrob(distribution_table, rows = NULL,
                       theme = gridExtra::ttheme_default(base_size = 14)) %>%
    export_plot("06-distribution_table", figure_width = 20, figure_height = 10)

  # variable distribution plot
  inspectdf::inspect_num(.dataset) %>%
    inspectdf::show_plot() %>%
    export_plot("07-distribution_plot")

  invisible(.dataset)
}

#' display_variable_correlation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports a plot of the correlation matrix for each variable, showing the
#' correlation values between each variable combination
#'
#' @seealso
#' This is an example of exploratory data analsyis using the [dlookr](https://github.com/choonghyunryu/dlookr)
#' and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, dataset for exploratory data analysis
#'
#' @return dataset returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(palmerpenguins)
#'   library(here)
#'   library(fs)
#'  })
#' })
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir.create("output")}
#'
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' display_variable_correlation(penguins_raw)
#' }
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
  png("output/08-correlation_plot.png", width = 600, height = 600, type = "cairo-png")
  .dataset %>%
    cor(use = "complete.obs", method = "pearson") %>%
    corrplot::corrplot(method = "number", number.digits = 3,
                       type = "upper", mar = c(0, 0, 2, 0),
                       title = "Correlation between numeric variables")
  dev.off()

  invisible(.dataset)
}

#' Run exploratory data analyses
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Export the collection of exploratory data analyses plots and tables. This includes
#' the summary, distribution and correlation of variables and presence of outliers
#' exported by default, any of which may be excluded in the export
#'
#' @seealso
#' This is an example of exploratory data analsyis using the [dlookr](https://github.com/choonghyunryu/dlookr)
#' and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages
#'
#' @family exploratory data analysis
#'
#' @param .dataset data frame, dataset for exploratory data analysis
#' @param summary boolean, display variable summary (FALSE to turn off)
#' @param outliers boolean, display outliers (FALSE to turn off)
#' @param distribution boolean, display variable distribution (FALSE to turn off)
#' @param correlation boolean, display variable correlation (FALSE to turn off)
#'
#' @return dataset returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(palmerpenguins)
#'   library(here)
#'   library(fs)
#'  })
#' })
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir.create("output")}
#'
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' display_variable_collection(penguins_raw)
#' }
display_variable_collection <- function(.dataset,
                                        summary = TRUE,
                                        outliers = TRUE,
                                        distribution = TRUE,
                                        correlation = TRUE) {

  # basic statistics of the variables
  if(summary) {
    display_variable_summary(.dataset)
  }

  # variable outliers
  if(outliers) {
    display_variable_outliers(.dataset)
  }

  # distribution of the numeric variables
  if(distribution) {
    display_variable_distribution(.dataset)
  }

  # correlation matrix of the variables
  if(correlation) {
    display_variable_correlation(.dataset)
  }

  invisible(.dataset)
}
