#https://rdrr.io/github/sgutreuter/SGmisc/src/R/dateFunctions.R

################################################################################
##     R SCRIPT: dateFunctions.R
##
##      PACKAGE: SGmisc
##
##  DESCRIPTION: Miscellaneous small functions for dates
##
##   WRITTEN BY: Steve Gutreuter
##               E-mail:  sgutreuter@gmail.com
################################################################################

#' Compute the midpoint between two dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning
#' date(s)
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#'
#'
#' @return A vector of class \code{Date} containing the midpoint date(s) between
#' \code{startdate} and \code{enddate}
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{\link[lubridate]{interval}}
#'
#' @examples
#' begin <- ymd("2020-06-21", "2021-06-21")
#' end <- ymd("2020-12-21", "2021-12-21")
#' mid_date(begin, end)
#'
#' @importFrom lubridate interval as_date int_start int_end
#' @export
mid_date <- function(startdate, enddate) {
  stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
  stopifnot(length(startdate) == length(enddate))
  idx <- enddate < startdate
  if (any(idx, na.rm = TRUE)) {
    enddate[idx] <- NULL
    startdate[idx] <- NULL
    cat(paste0("\nWARNING: NAs assigned to ", sum(idx, na.rm = TRUE),
               " inconsistent date pairs\n"))
  }
  res <- NULL
  if (length(startdate > 0)) {
    intobj <- lubridate::interval(startdate, enddate)
    res <- lubridate::as_date(lubridate::int_start(intobj) +
                                ((lubridate::int_end(intobj) -
                                    lubridate::int_start(intobj)) / 2))
  }
  res
}


#' Compute uniformly distributed random dates within intervals of dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning
#' date(s)
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#' @param ... Additional arguments passed to \code{base::sample}
#'
#' @return A  vector of elements of class \code{Date} containing uniformly
#' distributed dates in the closed interval (\code{startdate}, \code{enddate}).
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{\link[base]{sample}} \code{\link[base]{seq.Date}}
#'
#' @examples
#' begin <- ymd("2020-06-21", "2021-06-21")
#' end <- ymd("2020-12-21", "2021-12-21")
#' rand_date(begin, end)
#'
#' @export
rand_date <- function(startdate, enddate, ...) {
  stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
  stopifnot(length(startdate) == length(enddate))
  idx <- enddate < startdate
  if (any(idx, na.rm = TRUE)) {
    enddate[idx] <- NA
    startdate[idx] <- NA
    cat(paste0("\nWARNING: NAs assigned to ", sum(idx, na.rm = TRUE),
               " inconsistent date pairs\n"))
  }
  M <- length(startdate)
  res <- NULL
  if (M > 0) {
    res <- rep(as.Date(NA), M)
    for (i in 1:M) {
      if (!(is.na(startdate[i]) | is.na(enddate[i]))) {
        res[i] <- base::sample(x = seq.Date(from = startdate[i],
                                            to = enddate[i],
                                            by = "day"),
                               size = 1)
      } else {
        res[i]  <- NA
      }
    }
  }
  res
}
################################   END of FILE   ###############################