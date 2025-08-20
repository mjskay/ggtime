#' Looped coordinates
#'
#' @examples
#'
#' library(ggplot2)
#' p <- as_tsibble(USAccDeaths) |>
#'   ggplot(aes(x = index, y = value)) +
#'   geom_line()
#'
#' p
#' p + coord_loop(period = "year")
#'
#' @export
coord_loop <- function(
  loop = waiver(),
  time_loop = waiver(),
  time = "x",
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  coord = coord_cartesian()
) {
  ggplot2::ggproto(
    NULL,
    CoordLoop(coord),
    loop = loop,
    time_loop = time_loop,
    time = time,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordLoop <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordLoop",
    coord,

    setup_layout = function(layout, params) {
      coord$setup_layout(layout, params)
    },

    setup_data = function(data, params) {
      coord$setup_data(data, params)
    },

    setup_params = function(self, data) {
      ggproto_parent(coord, self)$setup_params(data)
    },

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
      if (ggplot2::is_waiver(self$loop)) {
        self$loop <- cut_axis_time(uncut_params, self$time, self$time_loop)
      }

      # Recalculate the panel parameters zoomed in on the first region.
      # Doing it this way should apply expand settings, etc, again.
      # (comment out this line to disable zooming for debugging)
      self$limits[[self$time]] <- c(
        # Restart at the first time point
        self$loop[1],
        # End at the longest time point in the loop
        self$loop[1] + max(diff(self$loop))
      )
      cut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )
      cut_params$time_cuts <- self$loop
      cut_params
    },

    draw_panel = function(self, panel, params, theme) {
      if (!ggplot2::check_device("clippingPaths")) {
        stop("Looped coordinates requires R v4.2.0 or higher.")
      }

      # Get cutpoints along the axis for dividing the panel grob into regions
      cuts <- params[[self$time]]$rescale(params$time_cuts)
      origin <- cuts[[1]]

      # Translate and superimpose the panel grob on itself repeatedly.
      # I attempted to use defineGrob() + useGrob() here to improve efficiency
      # (since theoretically it allows efficient repitition of a single grob
      # drawn off-screen), I couldn't figure out how to make the grob defined
      # by defineGrob() not be clipped by the first region it would theoretically
      # have been drawn into given where it is in the grob tree, which meant
      # it was always getting clipped. So I stuck to just repeatedly re-drawing
      # the same grob.
      .viewport = switch(self$time, x = viewport, y = function(x, y, ...) {
        viewport(y = x, x = y, ...)
      })
      panel_grob <- inject(grobTree(!!!panel))
      translated_panels <- lapply(
        head(cuts, -1),
        function(x) {
          grobTree(
            panel_grob,
            vp = .viewport(
              unit(origin - x, "npc"),
              unit(0, "npc"),
              just = c(0, 0)
              # Could clip here (as below) but that will clip inside the space
              # added by `expand = `, so probably better not to
              # clip = rectGrob(unit(x, "npc"), just = 0, width = unit(1, "npc"))
            )
          )
        }
      )

      # # Uncomment for debug info --- region boundaries and centers
      # widths <- diff(cuts)
      # centers <- rowMeans(embed(cuts, 2))
      # translated_panels <- c(
      #   translated_panels,
      #   list(
      #     rectGrob(x = unit(centers, "npc"), y = unit(rep(0.5, length(centers)), "npc"), width = unit(widths, "npc"), gp = gpar(col = "red", fill = NA)),
      #     pointsGrob(x = unit(centers, "npc"), y = unit(rep(0.5, length(centers)), "npc"), gp = gpar(col = "red"))
      #   )
      # )

      ggproto_parent(coord, self)$draw_panel(translated_panels, params, theme)
    }
  )
}


#' Get time cutpoints along a positional axis
#' @param params Panel params, e.g. as returned by `Coord$setup_panel_params()`
#' and passed to `Coord$draw_panel(params = ...)`
#' @param axis Axis to cut (`"x"` or `"y"`).
#' @param by Duration to cut by
#' @returns vector of time cutpoints
#' @noRd
cut_axis_time <- function(params, axis, by) {
  trans <- params[[axis]]$get_transformation()
  range <- params[[axis]]$limits
  time_range <- trans$inverse(range)
  time_cuts <- unique(c(
    seq(time_range[1], time_range[2], by = by),
    time_range[2]
  ))
  time_cuts
}
