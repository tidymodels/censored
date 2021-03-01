#' Number of days before a movie grosses $1M USD
#'
#' These data are a somewhat biased random sample of 551 movies released between
#' 2015 and 2018. Columns include
#'
#' * `title`: a character string for the movie title.
#' * `time`: number of days until the movie earns a million US dollars.
#' * `event`: a binary value for whether the movie reached this goal. About 94%
#'   of the movies had observed events.
#' * `released`: a date field for the release date.
#' * `distributor`: a factor with the the name of the distributor.
#' * `released_theaters`: the maximum number of theaters where the movie played
#'   in the first two weeks of release.
#' * `year`: the release year.
#' * `rated`: a factor for the Motion Picture Association film rating.
#' * `runtime`: the length of the movie (in minutes).
#' * A set of indicators columns for the movie genre (e.g. `action`, `crime`,
#'   etc.).
#' * A set of indicators for the language (e.g., `english`, `hindi`, etc.).
#' * A set of indicators for countries where the movie was released (e.g., `uk`,
#'   `japan`, etc.)
#'
#'
#' @name time_to_million
#' @aliases time_to_million
#' @docType data
#' @return \item{time_to_million}{a tibble}
#' @keywords datasets
NULL
