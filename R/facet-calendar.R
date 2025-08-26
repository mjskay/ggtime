#' Lay out panels in a calendar format
#'
#' @param date A variable that contains dates or an expression that generates
#' dates will be mapped in the calendar.
#' @param format A character string, such as `%Y-%b-%d` and `%a (%d)`, formatting
#' the display of facet strips. See `?strptime` for details.
#' @param week_start Day on which week starts following ISO conventions -
#' 1 means Monday (default), 7 means Sunday. You can set `lubridate.week.start`
#' option to control this parameter globally.
#' @param nrow,ncol Number of rows and columns defined for "monthly" calendar
#' layout. If `NULL`, it computes a sensible layout.
#' @param dir Direction of calendar: "h" for horizontal (the default) or "v" for
#' vertical.
#' @inheritParams ggplot2::facet_wrap
#' @importFrom gtable gtable_add_cols gtable_add_rows
#'
#' @details A monthly calendar is set up as a 5 by 7 layout matrix. Each month could
#' extend over six weeks but in these months is to wrap the last few days up
#' to the top row of the block.
#'
#' @rdname facet-calendar
#'
#' @author Earo Wang
#'
#' @export
#' @examples
#' \donttest{
#' fs <- dplyr::filter(
#'   tsibble::pedestrian,
#'   Date < as.Date("2015-05-01")
#' )
#'
#' fs |>
#'   ggplot(aes(x = Time, y = Count)) +
#'   geom_line(aes(colour = Sensor)) +
#'   facet_calendar(vars(Date), nrow = 2) +
#'   theme(legend.position = "bottom")
#' }
facet_calendar <- function(
  date,
  format = "%b %d",
  week_start = getOption("lubridate.week.start", 1),
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  shrink = TRUE,
  dir = "h",
  labeller = "label_value",
  strip.position = "top"
) {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  facet <- facet_wrap(
    ~.label,
    nrow = nrow,
    ncol = ncol,
    scales = scales,
    shrink = shrink,
    strip.position = strip.position
  )
  facet$params$date <- as_facet_date(date)
  facet$params$format <- format
  facet$params$week_start <- week_start
  facet$params$free <- free
  facet$params$dir <- dir
  facet$params$labeller <- labeller
  ggproto(NULL, FacetCalendar, shrink = shrink, params = facet$params)
}

#' @rdname facet-calendar
#' @format NULL
#' @usage NULL
#' @export
FacetCalendar <- ggproto(
  "FacetCalendar",
  FacetWrap,
  compute_layout = function(data, params) {
    eval_date <- eval_tidy(params$date, data = data[[1]])
    date_chr <- expr_text(params$date)

    if (!(inherits(eval_date, "Date"))) {
      abort(sprintf(
        "Argument `date` must be class 'Date', not '%s'.",
        class(eval_date)[[1]]
      ))
    }

    if (NROW(data[[1]]) == 0L) {
      abort("Facet calendar must have at least one date.")
    }
    layout <- setup_calendar.monthly(
      eval_date,
      dir = params$dir,
      week_start = params$week_start,
      nrow = params$nrow,
      ncol = params$ncol
    )
    n <- NROW(layout)

    layout %>%
      dplyr::mutate(
        !!date_chr := PANEL,
        .label = format.Date(PANEL, format = params$format),
        PANEL = factor(seq_len(n), levels = seq_len(n)),
        SCALE_X = ifelse(params$free$x, seq_len(n), 1L),
        SCALE_Y = ifelse(params$free$y, seq_len(n), 1L)
      )
  },

  map_data = function(data, layout, params) {
    date_chr <- expr_text(params$date)
    if (is_call(params$date)) {
      data <- dplyr::mutate(data, !!date_chr := !!params$date)
    }
    dplyr::left_join(data, layout, by = date_chr)
  },

  draw_panels = function(
    self,
    panels,
    layout,
    x_scales,
    y_scales,
    ranges,
    coord,
    data,
    theme,
    params
  ) {
    canvas <- ggproto_parent(FacetWrap, self)$draw_panels(
      panels,
      layout,
      x_scales,
      y_scales,
      ranges,
      coord,
      data,
      theme,
      params
    )

    space_x <- theme$panel.spacing.x %||% theme$panel.spacing
    row_spacer <- 2 * space_x
    space_y <- theme$panel.spacing.y %||% theme$panel.spacing
    col_spacer <- 2 * space_y

    # No idea why 28 and 26 are position in gtable (perhaps magic numbers)
    ncol <- max(layout$MCOL)
    nrow <- max(layout$MROW)
    if (params$dir == "h") {
      for (i in seq(28, by = 28, length.out = ncol - 1)) {
        canvas <- gtable_add_cols(canvas, width = col_spacer, pos = i)
      }
      for (j in seq(26, by = 26, length.out = nrow - 1)) {
        canvas <- gtable_add_rows(canvas, heights = row_spacer, pos = j)
      }
    } else {
      for (i in seq(21, by = 21, length.out = ncol - 1)) {
        canvas <- gtable_add_cols(canvas, width = col_spacer, pos = i)
      }
      for (j in seq(36, by = 36, length.out = nrow - 1)) {
        canvas <- gtable_add_rows(canvas, heights = row_spacer, pos = j)
      }
    }
    canvas
  }
)

as_facet_date <- function(x) {
  if (is_string(x)) {
    x <- sym(x)
  }
  if (is_formula(x)) {
    x <- f_rhs(x)
  }
  if (is_quosures(x)) {
    stopifnot(length(x) == 1)
    x <- get_expr(x[[1L]])
  }
  x
}

## Setting up different calendar layouts
# It builds a complete calendar layout, whilst using "left_join" with
# "date" variable
setup_calendar <- function(x, dir = "h", ...) {
  dir <- match.arg(dir, choices = c("h", "v"))
  UseMethod("setup_calendar")
}

setup_calendar.daily <- function(x, dir = "h", ...) {
  # x is a vector of unique dates
  x <- unique(x)
  mday_x <- lubridate::mday(x)
  month_x <- unique(x - mday_x + 1)
  nfacets <- length(month_x)
  seq_facets <- seq_len(nfacets)
  days_x <- unname(lubridate::days_in_month(month_x)) # d # unname() get rid of rlang warning
  counter <- map2(
    # g
    .x = month_x,
    .y = days_x,
    function(.x, .y) .x + 0:(.y - 1)
  )
  # if dir == "h"
  row_idx <- list(rep.int(seq_facets, days_x))
  col_idx <- lapply(days_x, seq_len)
  if (dir == "v") {
    # reverse col_idx and row_idx when direction is vertical
    col_tmp <- row_idx
    row_idx <- col_idx
    col_idx <- col_tmp
  }
  dplyr::tibble(
    ROW = rlang::flatten_int(row_idx),
    COL = rlang::flatten_int(col_idx),
    PANEL = do.call("c", counter)
  )
}

setup_calendar.weekly <- function(x, dir = "h", ...) {
  # x is a vector of unique dates
  x <- unique(x)
  init_counter <- lubridate::wday(min(x, na.rm = TRUE), week_start = 1)
  wk_x <- lubridate::isoweek(x)

  # only starts with Monday for ISO week
  col_idx <- lubridate::wday(x, week_start = 1)
  counter <- init_counter - 1 + x
  # if dir == "h"
  rle_x <- rle(wk_x)
  row_idx <- rep.int(seq_along(rle_x$values), rle_x$lengths)
  if (dir == "v") {
    # reverse col_idx and row_idx when direction is vertical
    col_tmp <- row_idx
    row_idx <- col_idx
    col_idx <- col_tmp
  }
  dplyr::tibble(ROW = row_idx, COL = col_idx, PANEL = counter)
}

setup_calendar.monthly <- function(
  x,
  dir = "h",
  week_start = 1,
  nrow = NULL,
  ncol = NULL,
  ...
) {
  # x is a vector of unique dates
  x <- unique(x)
  month_x <- unique(x - lubridate::mday(x) + 1)
  nfacets <- length(month_x)
  dims <- wrap_dims(nfacets, nrow = nrow, ncol = ncol)
  nrow <- dims[1]
  ncol <- dims[2]
  days_x <- lubridate::days_in_month(month_x) # d
  ndays <- 7
  max_wks <- 5
  ncells <- max_wks * ndays
  first_wday <- lubridate::wday(month_x, week_start = week_start) # k
  counter_date <- map2(
    .x = month_x,
    .y = days_x,
    function(.x, .y) .x + 0:(.y - 1)
  )
  counter <- map2(
    # g
    .x = first_wday,
    .y = days_x,
    function(.x, .y) .x + 0:(.y - 1)
  )
  row_idx <- lapply(
    # i
    counter,
    function(x) ifelse(x == ncells, max_wks, ceiling((x %% ncells) / ndays))
  )
  col_idx <- lapply(
    # j
    counter,
    function(x) ifelse(x %% ndays == 0, ndays, x %% ndays)
  )
  if (dir == "v") {
    # reverse col_idx and row_idx when direction is vertical
    col_tmp <- row_idx
    row_idx <- col_idx
    col_idx <- col_tmp
  }

  seq_facets <- seq_len(nfacets)
  m_idx <- ifelse(seq_facets %% ncol == 0, ncol, seq_facets %% ncol)
  last_rep <- m_idx[nfacets]
  n_idx <- rep.int(1:nrow, times = c(rep.int(ncol, nrow - 1), last_rep))
  if (dir == "h") {
    row_idx[] <- map2(
      .x = row_idx,
      .y = n_idx,
      function(.x, .y) .x + max_wks * (.y - 1)
    )
    col_idx[] <- map2(
      .x = col_idx,
      .y = m_idx,
      function(.x, .y) .x + ndays * (.y - 1)
    )
  } else {
    # dir = "v"
    row_idx[] <- map2(
      .x = row_idx,
      .y = n_idx,
      function(.x, .y) .x + ndays * (.y - 1)
    )
    col_idx[] <- map2(
      .x = col_idx,
      .y = m_idx,
      function(.x, .y) .x + max_wks * (.y - 1)
    )
  }
  dplyr::tibble(
    ROW = rlang::flatten_int(row_idx),
    COL = rlang::flatten_int(col_idx),
    MROW = rep.int(n_idx, days_x),
    MCOL = rep.int(m_idx, days_x),
    PANEL = do.call("c", counter_date),
    MPANEL = rep.int(seq_len(nfacets), days_x)
  )
}

map2 <- function(.x, .y, .f, ...) {
  Map(.f, .x, .y, ...)
}
