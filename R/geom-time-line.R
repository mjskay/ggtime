#' @export
geom_time_line <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "time_civil", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...) {
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

#' @format NULL
#' @usage NULL
#' @export
GeomTimeLine <- ggproto(
  "GeomTimeLine", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    # TODO
    # * Add multi-position if value is on the border of time zones
    # * Add gaps for implicit missing values

    # If the data is regular across a timezone change (offset) then draw a dashed line
    # Otherwise, there is a gap and there is no dashed timezone line


    tz_jumps <- c(FALSE, data$xoffset[-1] != data$xoffset[-nrow(data)])
    tz_jumps_i <- which(tz_jumps)

    data <- data[rep(seq_len(nrow(data)), 1L + tz_jumps),]
    tz_jump_dest <- seq_along(tz_jumps_i) + tz_jumps_i - 1
    data$x[tz_jump_dest] <- data$x[tz_jump_dest] + diff(data$xoffset[tz_jump_dest - (0:1)])
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
    if (nrow(munched) < 2) return(ggplot2::zeroGrob())

    # Work out grouping variables for grobs (e.g. rm implicit missing)
    # diff(x) == granularity(x) | irregular | timezone jump

    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    munched$fill <- arrow.fill %||% munched$colour

    # arrow <- ggplot2::repair_segment_arrow(arrow, munched$group)

    grid::segmentsGrob(
      munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
      default.units = "native", arrow = arrow,
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
