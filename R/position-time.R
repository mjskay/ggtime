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
      data$xoffset <- gmt_offset(scales$x$get_transformation()$inverse(
        data$x
      ))
    }
    if (inherits(scales$y, "ScaleContinuousDatetime")) {
      data$yoffset <- gmt_offset(scales$y$get_transformation()$inverse(
        data$y
      ))
    }

    # Apply offset to x/y positions
    if (!is.null(data$xoffset)) {
      data$x <- data$x + data$xoffset
    }
    if (!is.null(data$yoffset)) {
      data$y <- data$y + data$yoffset
    }

    # TODO: Update scale to enforce labelling in UTC time
    # (since tz is handled by position offsets)

    data
  }
)

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
    data$xoffset <- data$yoffset <- 0
    data
  }
)
