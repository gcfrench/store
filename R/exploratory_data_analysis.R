#' @title
#' Display a summary of the variables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the basic statistics of the variables within a data frame into a temporary
#' directory. This includes a plots of the prevalence of missing values and frequency
#' of category levels and a table image containing variable's type summary count of
#' the number of rows, missing values, unique values and zero values for each variable.
#'
#' @section Figures:
#' ![01-summary_table](01-summary_table.png)
#' ![02-missing_data](02-missing_data.png)
#' ![03-category_data](03-category_data.png)
#'
#' @seealso
#' This is an example of exploratory data analysis using the [dlookr](https://github.com/choonghyunryu/dlookr)
#' and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages.
#'
#' @family exploratory data analysis
#'
#' @param .dataset A data frame requiring exploratory data analysis.
#'
#' @return The data frame is returned invisibly so that the function can be
#' used in a piped workflow.
#'
#' @export
#'
#' @examples
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'   })
#' })
#'
#' suppressMessages({eda_variable_summary(penguins_raw)})
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(here)
#'   })
#' })
#'
#' file_move(path(tempdir(), "figures", "01-summary_table.png"),
#'           here("man", "figures", "01-summary_table.png"))
#'
#' file_move(path(tempdir(), "figures", "02-missing_data.png"),
#'           here("man", "figures", "02-missing_data.png"))
#'
#' file_move(path(tempdir(), "figures", "03-category_data.png"),
#'          here("man", "figures", "03-category_data.png"))
eda_variable_summary <- function(.dataset) {
  # export plot as image
  export_plot <- function(plot, plot_name,
                          figure_width = 6, figure_height = 6) {
    ggplot2::ggsave(filename = fs::path(tempdir(), "figures", stringr::str_glue("{plot_name}.png")),
                    plot = plot,
                    type = "cairo-png",
                    width = figure_width,
                    height = figure_height,
                    units = "in",
                    dpi = 72)
    invisible(plot)
  }

  # create temp directory
  fs::dir_create(fs::path(tempdir(), "figures"))

  # check for types
  check_numeric <- any(c("numeric") %in% (dlookr::diagnose(.dataset)$types))
  check_nominal <- any(c("factor", "character", "Date") %in% (dlookr::diagnose(.dataset)$types))

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
    nominal_table <- dlookr::diagnose(.dataset) %>%
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

#' @title
#' Display the variable outliers
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the variable outliers within a data frame into a temporary directory.
#' This includes displaying outliers in a box plot and summary statistics including
#' count outliers and mean of each variable with outliers included and excluded.
#'
#' @section Figures:
#' ![04-outliers_table](04-outliers_table.png)
#' ![05-variable_outliers](05-variable_outliers.png)
#'
#' @inherit eda_variable_summary return seealso
#'
#' @family exploratory data analysis
#'
#' @inheritParams eda_variable_summary
#'
#' @inherit eda_variable_summary return return
#'
#' @export
#'
#' @examples
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'   })
#' })
#'
#' suppressMessages({eda_variable_outliers(penguins_raw)})
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(here)
#'   })
#' })
#'
#' file_move(path(tempdir(), "figures", "04-outliers_table.png"),
#'           here("man", "figures", "04-outliers_table.png"))
#'
#' file_move(path(tempdir(), "figures", "05-variable_outliers.png"),
#'           here("man", "figures", "05-variable_outliers.png"))
eda_variable_outliers <- function(.dataset) {

  # export plot as image
  export_plot <- function(plot, plot_name,
                          figure_width = 6, figure_height = 6) {
    ggplot2::ggsave(filename = fs::path(tempdir(), "figures", stringr::str_glue("{plot_name}.png")),
                    plot = plot,
                    type = "cairo-png",
                    width = figure_width,
                    height = figure_height,
                    units = "in",
                    dpi = 72)
    invisible(plot)
  }

  # create temp directory
  fs::dir_create(fs::path(tempdir(), "figures"))

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

#' @title
#' Display the distribution of the variables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports the distribution of the numeric variables within a data frame into a temporary
#' directory. This includes a histogram for each variable and table of summary statistics,
#' including the range, quartiles, mean, medium, standard deviation, standard error of
#' the mean, level of skewness, kurtosis and normality.
#'
#' @section Figures:
#' ![06-distribution_table](06-distribution_table.png)
#' ![07-distribution_plot](07-distribution_plot.png)
#'
#' @inherit eda_variable_summary return seealso
#'
#' @family exploratory data analysis
#'
#' @inheritParams eda_variable_summary
#'
#' @inherit eda_variable_summary return return
#'
#' @export
#'
#' @examples
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'   })
#' })
#'
#' suppressMessages({eda_variable_distribution(penguins_raw)})
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(here)
#'   })
#' })
#'
#' file_move(path(tempdir(), "figures", "06-distribution_table.png"),
#'           here("man", "figures", "06-distribution_table.png"))
#'
#' file_move(path(tempdir(), "figures", "07-distribution_plot.png"),
#'           here("man", "figures", "07-distribution_plot.png"))
eda_variable_distribution <- function(.dataset) {

  # export plot as image
  export_plot <- function(plot, plot_name,
                          figure_width = 6, figure_height = 6) {
    ggplot2::ggsave(filename = fs::path(tempdir(), "figures", stringr::str_glue("{plot_name}.png")),
                    plot = plot,
                    type = "cairo-png",
                    width = figure_width,
                    height = figure_height,
                    units = "in",
                    dpi = 72)
    invisible(plot)
  }

  # create temp directory
  fs::dir_create(fs::path(tempdir(), "figures"))

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

#' @title
#' Display the correlation between the variables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Exports a plot of the correlation matrix for each variable into a temporary
#' directory, showing the correlation values between each variable combination.
#'
#' @section Figures:
#' ![08-correlation_plot](08-correlation_plot.png)
#'
#' @inherit eda_variable_summary return seealso
#'
#' @family exploratory data analysis
#'
#' @inheritParams eda_variable_summary
#'
#' @inherit eda_variable_summary return return
#'
#' @export
#'
#' @examples
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'   })
#' })
#'
#' suppressMessages({eda_variable_correlation(penguins_raw)})
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(here)
#'   })
#' })
#'
#' file_move(path(tempdir(), "figures", "08-correlation_plot.png"),
#'           here("man", "figures", "08-correlation_plot.png"))
eda_variable_correlation <- function(.dataset) {

  # create temp directory
  fs::dir_create(fs::path(tempdir(), "figures"))

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
  png(fs::path(tempdir(), "figures", "08-correlation_plot.png"),
      width = 600, height = 600, type = "cairo-png")
  .dataset %>%
    cor(use = "complete.obs", method = "pearson") %>%
    corrplot::corrplot(method = "number", number.digits = 3,
                       type = "upper", mar = c(0, 0, 2, 0),
                       title = "Correlation between numeric variables")
  dev.off()

  invisible(.dataset)
}

#' @title
#' Run the exploratory data analyses
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Export the collection of exploratory data analyses plots and tables into a temporary
#' directory. This includes the summary, distribution and correlation of variables
#' and presence of outliers exported by default, any of which may be excluded in the
#' export.
#'
#' @section Figures:
#' ![01-summary_table](01-summary_table.png)
#' ![02-missing_data](02-missing_data.png)
#' ![03-category_data](03-category_data.png)
#' ![04-outliers_table](04-outliers_table.png)
#' ![05-variable_outliers](05-variable_outliers.png)
#' ![06-distribution_table](06-distribution_table.png)
#' ![07-distribution_plot](07-distribution_plot.png)
#' ![08-correlation_plot](08-correlation_plot.png)
#'
#' @inherit eda_variable_summary return seealso
#'
#' @family exploratory data analysis
#'
#' @inheritParams eda_variable_summary
#' @param summary Use the default TRUE to display variable summary or FALSE to not display variable summary.
#' @param outliers Use the default TRUE to display outliers or FALSE to not display outliers.
#' @param distribution Use the default TRUE to display variable distribution or FALSE to not display variable distribution.
#' @param correlation Use the default TRUE to display variable correlation orFALSE to not display variable correlation.
#'
#' @inherit eda_variable_summary return return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # example from palmerpenguins
#' # https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'   })
#' })
#'
#' suppressMessages({eda_variable_collection(penguins_raw)})
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(here)
#'   })
#' })
#'
#' file_move(path(tempdir(), "figures", "01-summary_table.png"),
#'           here("man", "figures", "01-summary_table.png"))
#'
#' file_move(path(tempdir(), "figures", "02-missing_data.png"),
#'           here("man", "figures", "02-missing_data.png"))
#'
#' file_move(path(tempdir(), "figures", "03-category_data.png"),
#'          here("man", "figures", "03-category_data.png"))
#'
#' file_move(path(tempdir(), "figures", "04-outliers_table.png"),
#'           here("man", "figures", "04-outliers_table.png"))
#'
#' file_move(path(tempdir(), "figures", "05-variable_outliers.png"),
#'           here("man", "figures", "05-variable_outliers.png"))
#'
#' file_move(path(tempdir(), "figures", "06-distribution_table.png"),
#'           here("man", "figures", "06-distribution_table.png"))
#'
#' file_move(path(tempdir(), "figures", "07-distribution_plot.png"),
#'           here("man", "figures", "07-distribution_plot.png"))
#'
#' file_move(path(tempdir(), "figures", "08-correlation_plot.png"),
#'           here("man", "figures", "08-correlation_plot.png"))
#' }
eda_variable_collection <- function(.dataset,
                summary = TRUE,
                outliers = TRUE,
                distribution = TRUE,
                correlation = TRUE) {

  # create temp directory
  fs::dir_create(fs::path(tempdir(), "figures"))

  # basic statistics of the variables
  if(summary) {
    eda_variable_summary(.dataset)
  }

  # variable outliers
  if(outliers) {
    eda_variable_outliers(.dataset)
  }

  # distribution of the numeric variables
  if(distribution) {
    eda_variable_distribution(.dataset)
  }

  # correlation matrix of the variables
  if(correlation) {
    eda_variable_correlation(.dataset)
  }

  invisible(.dataset)
}
