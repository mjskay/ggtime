#' Calendar coordinates
#'
#' Create a coordinate system that wraps time series data over specified periods.
#'
#' @inheritParams coord_loop
#' @export
coord_calendar <- function(
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
    CoordCalendar(coord),
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
CoordCalendar <- function(coord) {
  # TODO: maybe don't want an inheritance relationship here
  # (would just need to factor out common setup_panel_params / draw_panel code)
  base_coord <- coord
  coord <- CoordLoop(coord)
  ggplot2::ggproto(
    "CoordCalendar",
    coord,

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      # Determine row for each cut region
      # TODO: Currently this just lays everything out in a column, but n_row
      # and/or time_rows presumably should come from somewhere that knows stuff
      # about the layout of the calendar.
      params$n_row <- length(params$time_cuts) - 1
      params$time_rows <- seq_len(params$n_row)

      params
    },

    render_fg = function(self, panel_params, theme) {
      fg <- ggproto_parent(coord, self)$render_fg(panel_params, theme)
      repeat_grob_in_rows(fg, panel_params$n_row, panel_params$is_flipped)
    },

    render_bg = function(self, panel_params, theme) {
      bg <- ggproto_parent(coord, self)$render_bg(panel_params, theme)
      repeat_grob_in_rows(bg, panel_params$n_row, panel_params$is_flipped)
    },

    render_axis_v = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_v(panel_params, theme)
      if (!panel_params$is_flipped) {
        # TODO: factor out (see note in repeat_grob_in_rows)
        height <- 1/panel_params$n_row
        axis_grobs <- lapply(axis_grobs, function(grob) gtable_col(
          "y_axis",
          replicate(panel_params$n_row, grob, simplify = FALSE),
          width = grobWidth(grob),
          heights = unit(rep(height, panel_params$n_row), "npc")
        ))
      }
      axis_grobs
    },

    render_axis_h = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_h(panel_params, theme)
      if (panel_params$is_flipped) {
        # TODO: factor out (see note in repeat_grob_in_rows)
        width <- 1/panel_params$n_row
        axis_grobs <- lapply(axis_grobs, function(grob) gtable_row(
          "x_axis",
          replicate(panel_params$n_row, grob, simplify = FALSE),
          height = grobHeight(grob),
          widths = unit(rep(width, panel_params$n_row), "npc")
        ))
      }
      axis_grobs
    }
  )
}

#' Replicate a grob in rows
#' @param grob a grob
#' @param n_row number of rows
#' @param is_flipped if `TRUE`, repeat in columns
repeat_grob_in_rows <- function(grob, n_row, is_flipped) {
  # TODO: should be able to factor out a common implementation between this,
  # render_axis_h, and render_axis_v (gtable was being finicky for me about z
  # orders so I left this for now)
  height <- 1/n_row
  .viewport <- flip_grid_fun(viewport, is_flipped)
  ys <- 1 - seq_len(n_row)/n_row
  height <- unit(1 / n_row, "npc")
  grobs <- lapply(ys, function(y) {
    grobTree(grob, vp = .viewport(x = unit(0, "npc"), y = unit(y, "npc"), just = c(0, 0), height = height))
  })
  inject(grobTree(!!!grobs))
}

