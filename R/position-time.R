position_time_civil <- function() {
  PositionTimeCivil
}

#' @rdname PositionTime
#' @format NULL
#' @usage NULL
#' @export
PositionTimeCivil <- ggproto("PositionTimeCivil", Position,
  required_aes = c("x|y"),

  default_aes = aes(xoffset = 0, yoffset = 0),

  # TODO: Check and error if multiple times are given in the scale, since
  # individual timezones from each calendar need to be used here.
  compute_panel = function(self, data, params, scales) {
    # scales$x$get_transformation()$inverse(data$x)
    data$x <- data$x + data$xoffset
    data$y <- data$y + data$yoffset

    # TODO: Update scale to enforce labelling in UTC time
    # (since tz is handled by position offsets)

    # Has access to all aes
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
PositionTimeAbsolute <- ggproto("PositionTimeAbsolute", Position,
  required_aes = c("x|y", "xoffest|yoffset"),

  compute_panel = function(self, data, params, scales) {
    data
  }
)
