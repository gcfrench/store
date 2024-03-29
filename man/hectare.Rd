% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gridref.R
\name{hectare}
\alias{hectare}
\alias{hectare.gridref}
\title{Get the 100m grid reference}
\usage{
hectare(x)

\method{hectare}{gridref}(grid_reference)
}
\arguments{
\item{grid_reference}{A Great British or Irish grid reference character string with class gridref.}
}
\value{
The grid reference at 100m resolution.
}
\description{
This function returns the 100m grid reference for a higher precision grid reference.
}
\details{
It can check either British or Irish grid references up to 10 figure (1m precision),
including tetrads (2000m precision).
}
\section{REST API}{

A REST API endpoint is available once a Plumber router has been created and can be
requested by adding \strong{_api} to the end of the function.

The API returns "" instead of NULL so that it may be used with the dplyr::mutate function.
Wrap the API call with dplyr::na_if to convert "" to NA.

\if{html}{\out{<div class="sourceCode">}}\preformatted{suppressPackageStartupMessages(\{
  suppressWarnings(\{
    library(plumber)
  \})
\})
pr("../R/gridref_api.R") \%>\%
  pr_run(port = 8000)
}\if{html}{\out{</div>}}
}

\examples{
 suppressPackageStartupMessages({
   suppressWarnings({
     library(dplyr)
     library(janitor)
   })
 })

 grid_references <- nbn_demonstration_dataset \%>\%
   clean_names() \%>\%
   slice_tail(n = 15) \%>\%
   select(grid_reference) \%>\%
   mutate(grid_reference = as_gridref(grid_reference))


# add one_hundred_m column
suppressPackageStartupMessages({
  suppressWarnings({
    library(dplyr)
  })
})

grid_references \%>\%
  rowwise() \%>\%
  mutate(one_hundred_m = hectare(grid_reference))
}
\seealso{
Other grid reference functions: 
\code{\link{easting}()},
\code{\link{gridsquare_geometry}()},
\code{\link{hectad}()},
\code{\link{monad}()},
\code{\link{northing}()},
\code{\link{pentad}()},
\code{\link{precision}()},
\code{\link{projection}()},
\code{\link{ten_metre}()},
\code{\link{tetrad}()}
}
\concept{grid reference functions}
