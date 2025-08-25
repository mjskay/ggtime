#' Calendar coordinates
#'
#' @examples
#'
#' library(ggplot2)
#' x <- 1:100
#' p <- tibble::tibble(
#'   time = Sys.time() + x*3600,
#'   sin = sin(x/pi)
#' ) |>
#'   ggplot(aes(x = time, y = sin)) +
#'   geom_line()
#'
#' p
#' p + coord_calendar(coord=coord_cartesian())
#' p + coord_calendar(coord=coord_polar())
#'
#' @export
coord_calendar <- function(
  period = "week",
  time = "x",
  xlim = NULL,
  ylim = NULL,
  expand = TRUE,
  default = FALSE,
  clip = "on",
  coord = coord_cartesian()
) {
  # Start by assuming time = "x"
  # TODO: Generalise to vertical calendars with time = "y"
  stopifnot(time == "x")

  ggplot2::ggproto(
    NULL,
    CoordCalendar(coord),
    period = period,
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
CoordCalendar <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordCalendar",
    coord,
    setup_layout = function(layout, params) coord$setup_layout(layout, params),
    setup_data = function(data, params) {
      coord$setup_data(data, params)
    },
    render_fg = function(panel_params, theme) {
      fg <- coord$render_fg(panel_params, theme)
      fg
    },
    render_bg = function(self, panel_params, theme) {
      bg <- ggproto_parent(coord, self)$render_bg(panel_params, theme)
      return(bg)

      g <- gtable::gtable(
        widths = rep(grid::grobWidth(bg), 2),
        heights = rep(grid::grobHeight(bg), 3)
      )

      return(gtable::gtable_add_grob(
        g,
        rep(list(bg), 4),
        c(1:2, 1:2),
        c(1:2, 2:1) #rep(1:2, each = 3), rep(1:3, each = 2)
      ))

      return(gtable::gtable_add_grob(
        g,
        rep(list(bg), 6),
        rep(1:2, each = 3),
        rep(1:3, each = 2)
      ))

      nrow <- 3
      ncol <- 2
      make_bgs <- function(i, bg) {
        row <- (i - 1) %/% ncol + 1
        col <- (i - 1) %% ncol + 1
        x_pos <- (col - 0.5) / ncol # Normalised x-coordinate
        y_pos <- 1 - (row - 0.5) / nrow # Normalised y-coordinate (inverted y)

        gTree(
          children = gList(bg),
          vp = viewport(
            x = x_pos,
            y = y_pos,
            width = 1 / ncol,
            height = 1 / nrow,
            just = "center"
          )
        )
      }

      gTree(children = do.call(gList, lapply(1:6, make_bgs, bg)))
    },
    #
    # reposition_axis = function(axis, row = 1, col = 1, nrow = 1, ncol = 1) {
    #   # zeroGrob, return unmodified axis
    #   if (is.null(axis$children$axis)) return(axis)
    #
    #   # row <- (i-1)%/%ncol + 1
    #   # col <- (i-1)%%ncol + 1
    #   x_pos <- (col - 0.5) / ncol  # Normalised x-coordinate
    #   y_pos <- 1 - (row - 0.5) / nrow  # Normalised y-coordinate (inverted y)
    #
    #   # Modify axis
    #   child_axis <- .mapply(function(x, y) {
    #     out <- axis$children$axis
    #
    #     out$vp <- viewport(
    #       x = unit(x, "npc"), width = unit(1 / ncol, "npc"),
    #       y = unit(y, "npc"), height = unit(1 / nrow, "npc"),
    #     )
    #
    #     # axis
    #     gTree(children = gList(out))
    #   }, list(x = x_pos, y = y_pos), NULL)
    #
    #
    #   # TODO: Directly editing the child "axis" feels hacky, there must be better ways.
    #   axis$children$axis <- gTree(children = do.call(gList, child_axis))
    #   axis
    # },
    # render_axis_v = function(self, panel_params, theme) {
    #   axis_v <- ggproto_parent(coord, self)$render_axis_v(panel_params, theme)
    #
    #   axis_v$left <- self$reposition_axis(axis_v$left, row = 1:3, nrow = 3)
    #   axis_v$right <- self$reposition_axis(axis_v$right, row = 1:3, nrow = 3)
    #
    #   axis_v
    # },
    # render_axis_h = function(self, panel_params, theme) {
    #   axis_h <- ggproto_parent(coord, self)$render_axis_h(panel_params, theme)
    #
    #   axis_h$top <- self$reposition_axis(axis_h$top, col = 1:2, ncol = 2)
    #   axis_h$bottom <- self$reposition_axis(axis_h$bottom, col = 1:2, ncol = 2)
    #
    #   axis_h
    # },
    draw_panel = function(self, panel, params, theme) {
      if (!ggplot2::check_device("clippingPaths")) {
        stop("Calendar coordinates requires R v4.2.0 or higher.")
      }
      browser()

      # Get time range from x-axis for cutting
      tx <- params$x$get_transformation()
      time_range <- tx$inverse(params$x$limits)
      time_cuts <- unique(c(
        seq(time_range[1], time_range[2], by = self$period),
        time_range[2]
      ))
      plot_cuts <- rescale(tx$transform(time_cuts), c(0, 1))

      widths <- diff(plot_cuts)
      centers <- rowMeans(embed(plot_cuts, 2))

      panel <- ggproto_parent(coord, self)$draw_panel(panel, params, theme)

      # grid.draw(panel$children$layout)
      # grid.draw(panel$children$GRID.polyline.88)

      plot.new()
      grid.draw(panel)

      src <- circleGrob(3:4 / 5, r = .3, gp = gpar(col = NA, fill = 2))
      grid.draw(src)
      grid.draw(groupGrob(groupGrob(src), "dest.in", panel))

      z <- groupGrob(
        groupGrob(rectGrob(x = centers[1], width = widths[1])),
        "dest.in",
        panel
      )

      grid.draw(useGrob(z, viewportTranslate))
      z$src

      grid.draw(rectGrob(x = centers[1], width = widths[1]))

      grid.draw(
        groupGrob(
          groupGrob(rectGrob(x = centers[1], width = widths[1])),
          "dest.in",
          panel
        ),
      )
      grid.draw(
        groupGrob(
          panel,
          vp = viewport(
            x = centers[1],
            width = widths[1],
            xscale = c(0, widths[1])
          )
        )
      )

      cal_grobs <- .mapply(
        function(x, w) {
          groupGrob(groupGrob(rectGrob(x = x, width = w)), "dest.in", panel)
        },
        list(centers, widths),
        NULL
      )

      cal_grobs
      gt <- gtable(widths = unit(1, "null"), heights = unit(c(1, 1), "null"))
      gt <- gtable_add_grob(gt, cal_grobs[1], 1, 1)
      gt <- gtable_add_grob(gt, cal_grobs[2], 2, 1)
      grid.draw(gt)

      plot.new()
      grid.group(panel)

      r <- rectGrob(
        1 / 3,
        2 / 3,
        width = .5,
        height = .5,
        gp = gpar(fill = "black")
      )
      c <- circleGrob(2 / 3, 1 / 3, r = .3, gp = gpar(fill = "black"))
      gt <- gTree(children = gList(r, c))
      grid.group(gt)
      grid.group(c, "dest.out", r)
      plot.new()
      mask <- rectGrob(gp = gpar(col = NA, fill = rgb(0, 0, 0, .5)))
      grid.text("background")
      pushViewport(viewport(mask = mask))

      gg <- ggplotGrob(p)
      grid.group(gg)

      plot.new()
      src <- circleGrob(3:4 / 5, r = .3, gp = gpar(col = NA, fill = 2))
      dst <- circleGrob(1:2 / 5, r = .3, gp = gpar(col = NA, fill = 3))
      grid.draw(groupGrob(groupGrob(src), "dest.in", panel))

      g <- gtable::gtable(
        widths = rep(grid::grobWidth(panel), 2),
        heights = rep(grid::grobHeight(panel), 3)
      )

      return(gtable::gtable_add_grob(
        g,
        rep(list(panel), 4),
        c(1:2, 1:2),
        c(1:2, 2:1) #rep(1:2, each = 3), rep(1:3, each = 2)
      ))
      return(gtable::gtable_add_grob(
        g,
        rep(list(panel), 6),
        rep(1:2, each = 3),
        rep(1:3, each = 2)
      ))
      # return(panel)

      nrow <- 3
      ncol <- 2
      make_panels <- function(i, panel) {
        row <- (i - 1) %/% ncol + 1
        col <- (i - 1) %% ncol + 1
        x_pos <- (col - 0.5) / ncol # Normalised x-coordinate
        y_pos <- 1 - (row - 0.5) / nrow # Normalised y-coordinate (inverted y)

        gTree(
          children = gList(panel),
          vp = viewport(
            x = x_pos,
            y = y_pos,
            width = 1 / ncol,
            height = 1 / nrow,
            just = "center"
          )
        )
      }

      gTree(children = do.call(gList, lapply(1:6, make_panels, panel)))
      # panel
    },

    setup_params = function(self, data) {
      ggproto_parent(coord, self)$setup_params(data)
    },

    # Move the data to the calendar sub-coord positions?
    transform = function(self, data, panel_params) {
      # data[[self$time]]

      # Panel limits
      if (!is.null(data$PANEL)) {
        # browser()
        tx <- panel_params$x$get_transformation()
        as.Date(tx$inverse(data$x))
      }
      # panel_params$x$get_transformation()$inverse(panel_params$x$limits)

      data <- ggproto_parent(coord, self)$transform(data, panel_params)

      # data$x <- data$x/2
      # data$y <- data$y/3

      data
    },
  )
}
