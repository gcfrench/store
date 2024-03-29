% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploratory_data_analysis.R
\name{eda_variable_distribution}
\alias{eda_variable_distribution}
\title{Display the distribution of the variables}
\usage{
eda_variable_distribution(.dataset)
}
\arguments{
\item{.dataset}{A data frame requiring exploratory data analysis.}
}
\value{
The data frame is returned invisibly so that the function can be
used in a piped workflow.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Exports the distribution of the numeric variables within a data frame into a temporary
directory. This includes a histogram for each variable and table of summary statistics,
including the range, quartiles, mean, medium, standard deviation, standard error of
the mean, level of skewness, kurtosis and normality.
}
\section{Figures}{

\if{html}{\figure{06-distribution_table.png}{options: width=100\%}}
\if{html}{\figure{07-distribution_plot.png}{options: width=80\%}}
}

\examples{
# example from palmerpenguins
# https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
suppressPackageStartupMessages({
  suppressWarnings({
    library(palmerpenguins)
  })
})

suppressMessages({eda_variable_distribution(penguins_raw)})

# move figures from temporary directory
suppressPackageStartupMessages({
  suppressWarnings({
    library(fs)
    library(here)
  })
})
if(dir_exists(here("man", "figures"))) {
  file_move(path(tempdir(), "figures", "06-distribution_table.png"),
            here("man", "figures", "06-distribution_table.png"))

  file_move(path(tempdir(), "figures", "07-distribution_plot.png"),
            here("man", "figures", "07-distribution_plot.png"))
}
}
\seealso{
Other exploratory data analysis: 
\code{\link{eda_variable_collection}()},
\code{\link{eda_variable_correlation}()},
\code{\link{eda_variable_outliers}()},
\code{\link{eda_variable_summary}()}
}
\concept{exploratory data analysis}
