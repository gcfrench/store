#' Parent R6 class representing a counter
#'
#' @description
#'
#' @details
#'
counter <- R6::R6Class("counter",
  private = list(

    #' @field start The starting count number.
    ..start = NULL,

    #' @field end The end count number.
    ..end = NULL,

    #' @field count The current count number.
    ..count = NULL,

    #' @field increment Whether to increase (add) or decrease (subtract) count number.
    ..increment = NULL,

    #' @field pad The number of digits for count number.
    ..pad = NULL,

    #' @field status Whether the counter is active (ON) or inactive (OFF).
    ..status = "ON"
  ),
  public = list(

    #' @description
    #' Increments count number depending on increment value (add or subtract). Changes
    #' status to OFF if count number has reached the end count number.
    counter = function() {
      if (private$..increment == "add") {
        private$..count <- private$..count + 1
      } else if (private$..increment == "subtract") {
        private$..count <- private$..count - 1
      }
      if (private$..count == private$..end) {
        private$..status <- "OFF"
      }
    },
    #' @description
    #' Displays count number padded with zeros.
    display = function() {
       stringr::str_c(stringr::str_pad(private$..count, private$..pad, pad = "0"))
    },
    #' @description
    #' Empty function not used on finalizing counter
    finalize = function() {
    }
  ),
  active = list(
    #' @description
    #' Returns counter status (ON or OFF)
    status = function() {
      private$..status
    }
  )
)

#' Child R6 class representing incrementing counter by adding count number.
counter_add <- R6::R6Class("counter_add",
  inherit = counter,
  public = list(
    #' @description
    #' Increments the counter by adding count number, defining maximum count.
    #'
    #' @param limit Maximum count as an integer.
    #'
    #' @return
    #' A new counter object
    initialize = function(limit) {
      private$..end <- limit
      private$..start <- 0L
      if (nchar(as.character(limit)) < 4) {
        private$..pad <- 4
      } else {
        private$..pad <- nchar(as.character(limit))
      }
      private$..count <- private$..start
      private$..increment <- "add"
      message(paste0(self$display(), " tally counter"))
    }
  )
)

#' Child R6 class representing incrementing counter by subtracting count number.
counter_subtract <- R6::R6Class("counter_subtract",
   inherit = counter,
   public = list(
     #' @description
     #' Increments the counter by subtracting count number, defining maximum count.
     #'
     #' @inheritParams counter_add
     #'
     #' @inheritSection counter_add return return
     initialize = function(limit) {
       private$..end <- 0L
       private$..start <- limit
       if (nchar(as.character(limit)) < 4) {
           private$..pad <- 4
       } else {
           private$..pad <- nchar(as.character(limit))
       }
       private$..count <- private$..start
       private$..increment <- "subtract"
       message(paste0(self$display(), " tally counter"))
       }
    )
)

#' @title
#' Count the number of remaining interactions
#'
#' @description
#' The main goal of the tally_counter is to provide a quick and easy way to monitor
#' progress whilst iterating through a data frame, applying a function to each row
#' at a time. This can be useful when the time taken for this step is sufficiently
#' long enough to run the script in the background, coming back to the console at
#' regular intervals to check the progress. To help with monitoring this progress,
#' the number of rows remaining can be displayed in the console so that you can see
#' at a glance how far the script has progressed and how far there is left to go.
#'
#' The counter is initiated by passing the data frame into this tally_counter function.
#' This can be done within a pipeline containing the iteration step, for example in
#' conjunction with the tidyverse suite of packages. It is used in conjunction with
#' the click function which updates the counter, either decreasing or increasing
#' the count by one. This up-to-date count can then be displayed in the console.
#'
#' On initiation the counter is set to the count to the number of rows to be iterated
#' through, counting downwards to zero during the iteration step. This behaviour can be
#' changed through the type argument so that instead of counting downwards the counter
#' counts upwards from zero to the total number of iterations.
#'
#' The number of digits displayed by the counter is set to four to mimic the appearance
#' of a real tally counter. However this number of digits is not fixed and will increase
#' to accommodate increasing number of iterations, so for example above 9,999 iterations
#' the number of digits increases to five, above 99,999 iterations six digits will be
#' displayed and so on.
#'
#' @param data a data frame to be used in the iteration
#' @param type character string indicating type of counter to use, either adding (add)
#' or subtracting (default = subtract) counts
#'
#' @return the data frame is returned so that the function is pipe friendly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'     library(dplyr)
#'     library(stringr)
#'     library(purrr)
#'   })
#' })
#'
#' penguin_stats <- function(...) {
#'
#'   # get row
#'   data <- list(...)
#'
#'   # get penguins stats
#'   species <- purrr::pluck(data, "species")
#'   island <- purrr::pluck(data, "island")
#'   body_mass_g <- purrr::pluck(data, "body_mass_g")
#'   sex <- purrr::pluck(data, "sex")
#'   year <- purrr::pluck(data, "year")
#'
#'   # print penguin stats
#'   message(stringr::str_glue("{click()} : The body mass for the {sex} {species} penguin recorded in {year} on {island} island is {body_mass_g} grams"))
#'
#' }
#' penguin_stats_slow <- slowly(penguin_stats, rate_delay(0.25))
#'
#' penguins %>%
#'   slice_sample(n = 25) %>%
#'   arrange(desc(body_mass_g)) %>%
#'   tally_counter(type = "add") %>%
#'   pwalk(penguin_stats_slow)
#' }
tally_counter <- function(data, ...) {

  # get function arguments
  arg_list <- list(...)
  if (any(class(data) == "data.frame")) {
    max_limit <- nrow(data)
  } else {
    max_limit <- length(data)
  }

  # create counter object in new counter_env environment
  assign("counter_env", new.env(parent = emptyenv()), envir = globalenv())
  if (!length(arg_list)) {
    assign("counter_obj", counter_subtract$new(limit = max_limit), envir = counter_env, inherits = FALSE)
  } else if (arg_list$type == "add") {
    assign("counter_obj", counter_add$new(limit = max_limit), envir = counter_env, inherits = FALSE)
  } else if (arg_list$type == "subtract") {
    assign("counter_obj", counter_subtract$new(limit = max_limit), envir = counter_env, inherits = FALSE)
  } else {
    stop("Incorrect tally counter type, type must be either add or subtract")
  }

  # return data frame allowing use in piping
  invisible(data)
}

#' @title
#' Increment the tally counter
#'
#' @description
#' This function adds or subtracts one from the counter depending on the tally counter
#' type used, defined within the tally_counter function. It returns the number padded
#' to a minimum of four characters. Once counter has finished the environment and counter
#' object are removed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'     library(dplyr)
#'     library(stringr)
#'     library(purrr)
#'   })
#' })
#'
#' penguin_stats <- function(...) {
#'
#'   # get row
#'   data <- list(...)
#'
#'   # get penguins stats
#'   species <- purrr::pluck(data, "species")
#'   island <- purrr::pluck(data, "island")
#'   body_mass_g <- purrr::pluck(data, "body_mass_g")
#'   sex <- purrr::pluck(data, "sex")
#'   year <- purrr::pluck(data, "year")
#'
#'   # print penguin stats
#'   message(stringr::str_glue("{click()} : The body mass for the {sex} {species} penguin recorded in {year} on {island} island is {body_mass_g} grams"))
#'
#' }
#' penguin_stats_slow <- slowly(penguin_stats, rate_delay(0.25))
#'
#' penguins %>%
#'   slice_sample(n = 25) %>%
#'   arrange(desc(body_mass_g)) %>%
#'   tally_counter(type = "add") %>%
#'   pwalk(penguin_stats_slow)
#' }
click <- function() {

  # get counter object
  counter_obj <- get("counter_obj", envir = counter_env)

  # increment counter object and check current counter status
  counter_obj$counter()

  # display current counter count
  count <- counter_obj$display()

  # remove counter object and enviromnent if counter finished
  if (counter_obj$status == "OFF") {
      rm(counter_obj, envir = counter_env)
      rm(counter_env, envir = globalenv())
      gc()
  }

  return(count)
}
