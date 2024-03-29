% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploratory_data_analysis.R
\name{eda_variable_summary}
\alias{eda_variable_summary}
\title{Display a summary of the variables}
\usage{
eda_variable_summary(.dataset)
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

Exports the basic statistics of the variables within a data frame into a temporary
directory. This includes a plots of the prevalence of missing values and frequency
of category levels and a table image containing variable's type summary count of
the number of rows, missing values, unique values and zero values for each variable.
}
\section{Figures}{

\if{html}{\figure{01-summary_table.png}{options: width=80\%}}
\if{html}{\figure{02-missing_data.png}{options: width=80\%}}
\if{html}{\figure{03-category_data.png}{options: width=80\%}}
}

\examples{
# example from palmerpenguins
# https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
suppressPackageStartupMessages({
  suppressWarnings({
    library(palmerpenguins)
  })
})

suppressWarnings({
  suppressMessages({
    eda_variable_summary(penguins_raw)
  })
})

# move figures from temporary directory
suppressPackageStartupMessages({
  suppressWarnings({
    library(fs)
    library(here)
  })
})
if(dir_exists(here("man", "figures"))) {
  file_move(path(tempdir(), "figures", "01-summary_table.png"),
            here("man", "figures", "01-summary_table.png"))

  file_move(path(tempdir(), "figures", "02-missing_data.png"),
            here("man", "figures", "02-missing_data.png"))

  file_move(path(tempdir(), "figures", "03-category_data.png"),
            here("man", "figures", "03-category_data.png"))
}
}
\seealso{
This is an example of exploratory data analysis using the \href{https://github.com/choonghyunryu/dlookr}{dlookr}
and \href{https://alastairrushworth.github.io/inspectdf/}{inspectdf} packages.

Other exploratory data analysis: 
\code{\link{eda_variable_collection}()},
\code{\link{eda_variable_correlation}()},
\code{\link{eda_variable_distribution}()},
\code{\link{eda_variable_outliers}()}
}
\concept{exploratory data analysis}
