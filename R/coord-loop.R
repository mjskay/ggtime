#' Looped coordinates
#'
#' The looped coordinate system loops the cartesian coordinate system around
#' specific loop points. This is particularly useful for visualising seasonal
#' patterns that repeat over calendar periods, since the shape of seasonal
#' patterns can be more easily seen when superimposed on top of each other.
#'
#' @param loops Loop the time scale around a calendrical granularity, one of:
#'   - `NULL` or `waiver()` for no looping (the default)
#'   - A `mixtime` vector giving time points at which the `time` axis should loop
#'   - A function that takes the limits as input and returns loop points as output
#' @param time_loops A duration giving the distance between temporal
#' loops like "2 weeks", or "10 years". If both `loops` and `time_loops` are
#' specified, `time_loops` wins.
#' @param time A string specifying which aesthetic contains the time variable that
#'   should be looped over. Default is `"x"`.
#' @param ljust Loop justification, a number between 0 and 1
#'   indicating where the lines between looped positions are drawn
#'   (0 indicates left, 1 indicates right, 0.5 indicates center).
#' @param xlim,ylim Limits for the x and y axes. `NULL` means use the default limits.
#' @param expand Logical indicating whether to expand the coordinate limits.
#'   Default is `FALSE`.
#' @param default Logical indicating whether this is the default coordinate system.
#'   Default is `FALSE`.
#' @param clip Should drawing be clipped to the extent of the plot panel?
#'   A setting of `"on"` (the default) means yes, and a setting of `"off"` means no.
#' @param clip_loops Should the drawing of each loop of the timescale be clipped to
#'   the breaks defined by `time_loops` and `ljust`?
#'   A setting of `"on"` (the default) means yes, and a setting of `"off"` means no.
#' @param coord The underlying coordinate system to use. Default is `coord_cartesian()`.
#'
#' @details
#' This coordinate system is particularly useful for visualizing seasonal or
#' cyclic patterns in time series data. It works by:
#'
#' \enumerate{
#'   \item Dividing the time axis into segments based on the specified loop period
#'   \item Translating each segment to overlay on the first segment
#'   \item Creating a visualization where multiple time periods are superimposed
#' }
#'
#' The coordinate system requires R version 4.2.0 or higher due to its use of
#' usage of clipping paths.
#'
#' @return A `Coord` ggproto object that can be added to a ggplot.
#'
#' @examples
#' library(ggplot2)
#' library(ggtime)
#'
#' # Basic usage with US accidental deaths data
#' uad <- tsibble::as_tsibble(USAccDeaths)
#' # Requires mixtime, POSIXct, or Date time types
#' uad$index <- as.Date(uad$index)
#'
#' p <- ggplot(uad, aes(x = index, y = value)) +
#'   geom_line()
#'
#' # Original plot
#' p
#'
#' # With yearly looping to show seasonal patterns
#' p + coord_loop(time_loop = "1 year")
#'
#' @export
coord_loop <- function(
  loops = waiver(),
  time_loops = waiver(),
  time = "x",
  ljust = 0.5,
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  clip_loops = "on",
  coord = coord_cartesian()
) {
  ggplot2::ggproto(
    NULL,
    CoordLoop(coord),
    loops = loops,
    time_loops = time_loops,
    time = time,
    ljust = ljust,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip,
    clip_loops = clip_loops
  )
}

#' @rdname ggplot2-ggproto
#' @keywords internal
CoordLoop <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordLoop",
    coord,

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      # We need to adjust the panel parameters so that the scale is zoomed in
      # on the first region (which we will translate all other regions onto in draw_panel).

      # Calculate the panel parmeters as normal (without cutting)
      # Need to do this so that user-defined limits, scale limits, expand, etc
      # are all appropriately taken into account
      uncut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      # Determine the cutpoints where we will loop
      if (is_waiver(self$loops)) {
        time_cuts <- cut_axis_time(
          uncut_params,
          self$time,
          self$time_loops,
          self$ljust
        )
      } else {
        time_cuts <- sort(unique(self$loops))
        time_cuts <- c(
          time_cuts - self$ljust,
          time_cuts[length(time_cuts)] + (1 - self$ljust)
        )
      }

      # Recalculate the panel parameters zoomed in on the first region.
      # Doing it this way should apply expand settings, etc, again.
      # (comment out this line to disable zooming for debugging)
      old_limits <- self$limits
      self$limits[[self$time]] <- c(
        # Restart at the first time point
        time_cuts[1],
        # End at the longest time point in the loop
        time_cuts[1] + max(diff(time_cuts))
      )
      cut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )
      self$limits <- old_limits

      cut_params$time_cuts <- time_cuts
      cut_params$time_rows <- rep.int(1L, length(cut_params$time_cuts) - 1)
      cut_params$n_row <- 1L
      cut_params$is_flipped <- isTRUE(self$time == "y")
      cut_params$uncut <- uncut_params
      cut_params
    },

    range = function(self, panel_params) {
      # range needs to be calculated on the uncut scale so that (e.g.)
      # the position of infinities is correct
      ggproto_parent(coord, self)$range(panel_params$uncut)
    },

    transform = function(self, data, panel_params) {
      reverse <- panel_params$reverse %||% "none"
      x <- panel_params$x[[switch(reverse, xy = , x = "reverse", "rescale")]]
      y <- panel_params$y[[switch(reverse, xy = , y = "reverse", "rescale")]]
      data <- transform_position(data, x, y)
      # need to use the full range for squish_infinite otherwise segments with
      # infinite endpoints are not repeated over rows in coord_calendar
      range <- transform_position(self$backtransform_range(panel_params), x, y)
      transform_position(
        data,
        function(x) scales::squish_infinite(x, range$x),
        function(y) scales::squish_infinite(y, range$y)
      )
    },

    draw_panel = function(self, panel, params, theme) {
      is_clipped = isTRUE(self$clip_loops %in% c("on", TRUE)) # could have stricter validation
      if (is_clipped && !ggplot2::check_device("clippingPaths")) {
        stop("Looped coordinates requires R v4.2.0 or higher.")
      }

      # Get cutpoints along the axis for dividing the panel grob into regions
      cuts <- params[[self$time]]$rescale(params$time_cuts)

      translated_panels <- translate_and_superimpose_grobs(
        panel,
        cuts,
        params$time_rows,
        params$n_row,
        params$is_flipped,
        is_clipped
      )

      ggproto_parent(coord, self)$draw_panel(translated_panels, params, theme)
    }
  )
}

#' Translate and superimpose grobs at specified cutpoints along x (or y) axis
#' @param grobs list of grobs
#' @param cuts x (or y if `is_flipped`) positions to cut along
#' @param rows vector with length = `length(cuts) - 1` giving
#' destination row id of each corresponding cut region (starting from 1).
#' @param n_row maximum row in the layout
#' @param is_flipped are the axes flipped? (so `cuts` are `y` positions).
#' @param is_clipped should output regions be clipped?
#' @noRd
translate_and_superimpose_grobs <- function(
  grobs,
  cuts,
  rows,
  n_row,
  is_flipped = FALSE,
  is_clipped = FALSE
) {
  origin <- cuts[[1]]
  xs <- cuts[-length(cuts)]
  widths <- diff(cuts)

  ys <- 1 - rows / n_row
  height <- 1 / n_row

  # Translate and superimpose the panel grob on itself repeatedly.
  # I attempted to use defineGrob() + useGrob() here to improve efficiency
  # (since theoretically it allows efficient repitition of a single grob
  # drawn off-screen), I couldn't figure out how to make the grob defined
  # by defineGrob() not be clipped by the first region it would theoretically
  # have been drawn into given where it is in the grob tree, which meant
  # it was always getting clipped. So I stuck to just repeatedly re-drawing
  # the same grob.
  .viewport <- flip_grid_fun(viewport, is_flipped)
  .rectGrob <- flip_grid_fun(rectGrob, is_flipped)
  grob <- inject(grobTree(!!!grobs))
  translated_grobs <- .mapply(
    function(x, y, width) {
      grobTree(
        grob,
        vp = .viewport(
          x = unit(origin - x, "npc"),
          y = unit(y, "npc"),
          height = unit(height, "npc"),
          just = c(0, 0),
          clip = if (is_clipped) {
            .rectGrob(
              unit(x, "npc"),
              y = unit(0, "npc"),
              width = unit(width, "npc"),
              height = unit(1, "npc"),
              just = c(0, 0)
            )
          } else {
            "inherit"
          }
        )
      )
    },
    list(xs, ys, widths),
    NULL
  )

  # # Uncomment for debug info --- region boundaries and centers
  # centers <- rowMeans(embed(cuts, 2))
  # translated_grobs <- c(
  #   translated_grobs,
  #   list(
  #     rectGrob(x = unit(centers, "npc"), y = unit(rep(0.5, length(centers)), "npc"), width = unit(widths, "npc"), gp = gpar(col = "red", fill = NA)),
  #     pointsGrob(x = unit(centers, "npc"), y = unit(rep(0.5, length(centers)), "npc"), gp = gpar(col = "red"))
  #   )
  # )

  translated_grobs
}

#' Flip the x and y axes of a grid function
#' @param f a \pkg{grid} function, like [viewport()] or [rectGrob()].
#' @param is_flipped should it be flipped?
#' @returns function with the same parameters as `f` but with positional axis
#' arguments (`x`, `y`, `width`, `height`, ...) swapped.
#' @noRd
flip_grid_fun <- function(f, is_flipped) {
  if (!is_flipped) {
    return(f)
  }
  new_f <- function(x, y, width, height, just, hjust, vjust, ...) {
    f(
      x = y,
      y = x,
      width = height,
      height = width,
      just = rev(just),
      hjust = vjust,
      vjust = hjust,
      ...
    )
  }
  formals(new_f)[c("x", "y", "width", "height", "hjust", "vjust")] <-
    formals(f)[c("y", "x", "height", "width", "vjust", "hjust")]
  new_f
}

#' Get time cutpoints along a positional axis
#' @param panel_params Panel params, e.g. as returned by `Coord$setup_panel_params()`
#' and passed to `Coord$draw_panel(params = ...)`
#' @param axis Axis to cut (`"x"` or `"y"`).
#' @param by Duration to cut by
#' @param ljust Loop justification, a number between 0 and 1
#' @returns vector of time cutpoints
#' @noRd
cut_axis_time <- function(panel_params, axis, by, ljust) {
  trans <- panel_params[[axis]]$get_transformation()
  range <- panel_params[[axis]]$limits
  time_range <- trans$inverse(range)
  time_cuts <- unique(c(
    seq(time_range[1] - ljust, time_range[2] + (1 - ljust), by = by),
    time_range[2]
  ))
  time_cuts
}
