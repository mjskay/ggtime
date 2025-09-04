#' Position adjustments for time data
#'
#' These position adjustments handle time data with different timezone behaviors:
#' * `position_time_civil()` applies timezone offsets to display times in their
#'   local civil time while keeping the underlying scale in UTC
#' * `position_time_absolute()` displays times in absolute UTC without any
#'   timezone adjustments
#'
#' @name PositionTime
#' @aliases position_time_civil position_time_absolute
#' @export position_time_civil position_time_absolute
NULL

#' @rdname PositionTime
#' @export
position_time_civil <- function() {
  PositionTimeCivil
}

#' @rdname PositionTime
#' @format NULL
#' @usage NULL
#' @export
PositionTimeCivil <- ggproto(
  "PositionTimeCivil",
  Position,
  required_aes = c("x|y"),

  default_aes = aes(xoffset = 0, yoffset = 0),

  # TODO: Check and error if multiple times are given in the scale, since
  # individual timezones from each calendar need to be used here.
  compute_panel = function(self, data, params, scales) {
    if (inherits(scales$x, "ScaleContinuousDatetime")) {
      data$xtimeoffset <- gmt_offset(scales$x$get_transformation()$inverse(
        data$x
      ))
      scales$x$timezone <- "UTC"
    }
    if (inherits(scales$y, "ScaleContinuousDatetime")) {
      data$ytimeoffset <- gmt_offset(scales$y$get_transformation()$inverse(
        data$y
      ))
      scales$y$timezone <- "UTC"
    }

    # Apply offset to x/y positions
    if (!is.null(data$xtimeoffset)) {
      data$x <- data$x + data$xtimeoffset
    }
    if (!is.null(data$ytimeoffset)) {
      data$y <- data$y + data$ytimeoffset
    }

    # TODO: Update mixtime scales to enforce labelling in UTC time
    # (since tz is handled by position offsets)

    data
  }
)

#' @rdname PositionTime
#' @export
position_time_absolute <- function() {
  PositionTimeAbsolute
}

#' @rdname PositionTime
#' @format NULL
#' @usage NULL
#' @export
PositionTimeAbsolute <- ggproto(
  "PositionTimeAbsolute",
  Position,
  required_aes = c("x|y"),

  compute_panel = function(self, data, params, scales) {
    data$xtimeoffset <- data$ytimeoffset <- 0
    data
  }
)
