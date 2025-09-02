#' Connect time observations with offset handling
#'
#' @description
#' `geom_time_line()` connects observations in order of the time variable, similar to
#' [ggplot2::geom_line()], but with special handling for time gaps and changes in time offsets.
#'
#' The geometry helps to visualise time with changing time offsets provided by the
#' `[x/y]timeoffset` aesthetics. Changes in time offsets are drawn using dashed lines,
#' which are most commonly used for timezone changes and daylight savings time transitions.
#' Timezone offsets are automatically used when times from the `mixtime` package are used
#' in conjunction with `position_time_civil()` positioning (the default).
#'
#' This geometry also respects implicit missing values in regular time series, and will
#' not connect temporal observations separated by gaps.
#'
#' The `group` aesthetic determines which cases are connected together.
#'
#' @aesthetics GeomTimeLine
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than
#'  combining with them. This is most useful for helper functions that define both
#'  data and aesthetics and shouldn't inherit behaviour from the default plot specification.
#' @param ... Other arguments passed on to [ggplot2::geom_line()].
#'
#' @seealso
#'  [position_time_civil()]/[position_time_absolute()] for civil and absolute time positioning.
#'
#'  [ggplot2::geom_line()]/[ggplot2::geom_path()] for standard line/path geoms in ggplot2.
#' @section Missing value handling:
#' `geom_time_line()` handles missing values similar to [ggplot2::geom_line()], but with
#' additional logic for implicit missing values in time series. Implicit missing
#' values (gaps in regular time intervals) are not connected, creating breaks in
#' the line without warnings.
#'
#' @section Changing time offsets:
#' The `xtimeoffset` and `ytimeoffset` aesthetics allow for visualization of time
#' offset changes, such as timezone transitions or daylight saving time changes.
#' When successive time offsets differ, a dashed line segment is drawn to show
#' the offset transition.
#'
#' @export
geom_time_line <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "time_civil",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      # orientation = orientation,
      ...
    )
  )
}

#' @keywords internal
GeomTimeLine <- ggproto(
  "GeomTimeLine",
  GeomPath,
  optional_aes = c("xtimeoffset", "ytimeoffset"),
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    arrow = NULL,
    arrow.fill = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE
  ) {
    # TODO
    # * Add dashed lines for y-axis offsets
    # * Add gaps for implicit missing values
    # * Use linear interpolation to calculate trend values at the timezone changes
    # * Add warning for sawtoothing (violation of uniqueness condition)

    # If the data is regular across a timezone change (offset) then draw a dashed line
    # Otherwise, there is a gap and there is no dashed timezone line

    tz_jumps <- c(FALSE, data$xtimeoffset[-1] != data$xtimeoffset[-nrow(data)])
    tz_jumps_i <- which(tz_jumps)

    # TODO: Bug doesn't work with multiple offset changes
    data <- data[rep(seq_len(nrow(data)), 1L + tz_jumps), ]
    tz_jump_dest <- seq_along(tz_jumps_i) + tz_jumps_i - 1
    data$x[tz_jump_dest] <- data$x[tz_jump_dest] +
      diff(data$xtimeoffset[tz_jump_dest - (0:1)])
    data$linetype[tz_jump_dest] <- 2L

    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn {class(self[1])}}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }

    # must be sorted on group
    # data <- data[order(data$group), , drop = FALSE]
    # data$bkwrd <- c(diff(data$x) < 0, FALSE)
    munched <- ggplot2::coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) {
      return(ggplot2::zeroGrob())
    }

    # Work out grouping variables for grobs (e.g. rm implicit missing)
    # diff(x) == granularity(x) | irregular | timezone jump

    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)

    munched$fill <- arrow.fill %||% munched$colour

    # arrow <- ggplot2::repair_segment_arrow(arrow, munched$group)

    grid::segmentsGrob(
      munched$x[!end],
      munched$y[!end],
      munched$x[!start],
      munched$y[!start],
      default.units = "native",
      arrow = arrow,
      gp = gg_par(
        col = alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$fill, munched$alpha)[!end],
        lwd = munched$linewidth[!end],
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
  },
)
