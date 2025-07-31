#' Position scales for mixtime data
#'
#' These are the default scales for mixtime vectors, which are usually used for
#' a plot automatically. To override the scales behaviour manually, use
#' `scale_*_mixtime`.
#'
#' @inheritParams ggplot2::scale_x_date
#' @param time_breaks A <duration> giving the distance between breaks like
#' "2 weeks", or "10 years". If both `breaks` and `time_breaks` are specified,
#' `time_breaks` wins.
#' @param time_minor_breaks A <duration> giving the distance between minor breaks like
#' "2 weeks", or "10 years". If both `minor_breaks` and `time_minor_breaks` are
#' specified, `time_minor_breaks` wins.
#' @param loops Loop the time scale around a calendrical granularity, one of:
#'   - `NULL` or `waiver()` for no wrapping (the default)
#'   - A `mixtime` vector giving positions of wrapping points
#'   - A function that takes the limits as input and returns wrapping points as
#'     output
#' @param time_loops A <mixtime::duration> giving the distance between temporal
#' loops like "2 weeks", or "10 years". If both `loops` and `time_loops` are
#' specified, `time_loops` wins.
#' @param warps Warp the time scale to have a consistent length, one of:
#'   - `NULL` or `waiver()` for no warping (the default)
#'   - A `mixtime` vector giving positions of warping points
#'   - A function that takes the limits as input and returns warping points as
#'     output
#' @param time_warps A <duration> giving the distance between temporal warping
#' like "2 weeks", or "10 years". If both `warps` and `time_warps` are
#' specified, `time_warps` wins.
#' @param civil_time The positioning of zoned time. If TRUE time is positioned
#' uses the local/wall time (the default), otherwise time will be shown in with
#' absolute/continuous UTC time.
#'
#' @export
#' @rdname scale_mixtime
scale_x_mixtime <- function(name = waiver(),
                         breaks = waiver(),
                         time_breaks = waiver(),
                         labels = waiver(),
                         time_labels = waiver(),
                         minor_breaks = waiver(),
                         time_minor_breaks = waiver(),
                         civil_time = TRUE,
                         # loops = waiver(),
                         # time_loops = waiver(),
                         warps = waiver(),
                         time_warps = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = scales::censor,
                         guide = waiver(),
                         position = "bottom",
                         sec.axis = waiver()) {

  sc <- mixtime_scale(
    aesthetics = ggplot_global$x_aes,
    transform = "mixtime",
    name = name,
    palette = identity,
    breaks = breaks,
    time_breaks = time_breaks,
    labels = labels,
    time_labels = time_labels,
    minor_breaks = minor_breaks,
    time_minor_breaks = time_minor_breaks,
    timezone = NULL,
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )
  debug(sc$get_labels)

  set_sec_axis(sec.axis, sc)
}


# scale_y_time <- function(name = waiver(),
#                          breaks = waiver(),
#                          cal_breaks = waiver(),
#                          labels = waiver(),
#                          cal_labels = waiver(),
#                          minor_breaks = waiver(),
#                          cal_minor_breaks = waiver(),
#                          timezone = NULL,
#                          limits = NULL,
#                          expand = waiver(),
#                          oob = scales::censor,
#                          guide = waiver(),
#                          position = "left",
#                          sec.axis = waiver()) {
#
#   sc <- datetime_scale(
#     c("y", "ymin", "ymax", "yend", "yintercept",
#       "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"),
#     "time",
#     name = name,
#     palette = identity,
#     breaks = breaks,
#     cal_breaks = cal_breaks,
#     labels = labels,
#     cal_labels = cal_labels,
#     minor_breaks = minor_breaks,
#     cal_minor_breaks = cal_minor_breaks,
#     timezone = timezone,
#     guide = guide,
#     limits = limits,
#     expand = expand,
#     oob = oob,
#     position = position
#   )
#
#   set_sec_axis(sec.axis, sc)
# }

#' @export
#' @keywords internal
mixtime_scale <- function(aesthetics, transform, trans = deprecated(),
                           palette, breaks = pretty_breaks(), minor_breaks = waiver(),
                           labels = waiver(), time_breaks = waiver(),
                           time_labels = waiver(),
                           time_minor_breaks = waiver(), timezone = NULL,
                           guide = "legend", call = caller_call(), ...) {
  call <- call %||% current_call()

  # Backward compatibility
  if (is.character(breaks)) breaks <- breaks_width(breaks)
  if (is.character(minor_breaks)) minor_breaks <- breaks_width(minor_breaks)

  if (!is_waiver(time_breaks)) {
    # check_string(cal_breaks)
    breaks <- breaks_width(time_breaks)
  }
  if (!is_waiver(time_minor_breaks)) {
    # check_string(cal_minor_breaks)
    minor_breaks <- breaks_width(time_minor_breaks/zip)
  }
  if (!is_waiver(time_labels)) {
    check_string(time_labels)
    labels <- function(self, x) {
      label_date(time_labels, tz)(x)
    }
  }

  # x/y position aesthetics should use ScaleContinuousMixtime; others use ScaleContinuous
  if (all(aesthetics %in% c(ggplot_global$x_aes, ggplot_global$y_aes))) {
    scale_class <- ScaleContinuousMixtime
  } else {
    scale_class <- ScaleContinuous
  }

  sc <- ggplot2::continuous_scale(
    aesthetics,
    palette = palette,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    guide = guide,
    transform = transform_mixtime(),
    trans = trans,
    call = call,
    ...,
    super = scale_class
  )

  # Range is hard-coded and not inherited by `super` in
  # `ggplot2::continuous_scale`, replace it.
  sc$range <- MixtimeRange$new()
  sc
}

#' @export
scale_type.mixtime <- function(x) c("mixtime", "continuous")

#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousMixtime <- ggproto(
  "ScaleContinuousMixtime", ScaleContinuous,
  secondary.axis = waiver(),
  timezone = NULL,
  range = MixtimeRange$new(),
  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- MixtimeRange$new()
    new
  },
  # Redefine ScaleContinuous$train for (possible) self$range scoping issue
  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    self$range$train(x, call = self$call)
  },
  transform_df = function(self, df) {
    # HACK
    # Add offsets for PositionTime[Civil/Absolute] here as "after_stat" since
    # Position$default_aes = aes(xoffset = stage(after_stat = f(x))) isn't
    # currently working in ggplot2
    missing_aes <- setdiff(names(PositionTimeCivil$default_aes), names(df))

    df <- ggproto_parent(ScaleContinuous, self)$transform_df(df)

    # Match missing_aes offset positions to transformed scales
    missing_aes_i <- match(missing_aes, paste0(names(df), "offset"))
    missing_aes_i <- missing_aes_i[!is.na(missing_aes_i)]

    df[missing_aes[missing_aes_i]] <- lapply(df[missing_aes_i], tz_offset)

    df
  },
  transform = function(self, x) {
    # Store common time type for default backtransformation, labels, and more.
    if (length(attr(x, "v")) != 1L) {
      cli::cli_abort("{.field mixtime} scales currently work with single-type vectors only.")
    }
    # TODO: make this optionally user-specified
    self$ptype <- attributes(attr(x, "v")[[1L]])


    # Possibly redefine self$trans with new info from `x`

    if (is_bare_numeric(x)) {
      cli::cli_abort(c(
        "A {.cls numeric} value was passed to a {.field mixtime} scale.",
        i = "Please use the mixtime package to create time values."
      ), call = self$call)
    }
    # ggtime:::calendar_wrap(x)
    ggproto_parent(ScaleContinuous, self)$transform(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    # TODO: Check functionality of self$oob
    # self$oob(x, limits)

    if (mixtime::is_mixtime(x)) x <- as.numeric(vecvec::unvecvec(x))
    x
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is_waiver(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is_waiver(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  # make_sec_title = function(self, title) {
  #   browser()
  #   if (!is_waiver(self$secondary.axis)) {
  #     self$secondary.axis$make_title(title)
  #   } else {
  #     ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
  #   }
  # }

)

# Inversion requires recollection of offset and regularity
# Warping between specific time points numeric 0-n for n warp points, decimals indicate time between warp points
transform_mixtime <- function () {
  # TODO: replace common_attr with ptype provided by scale
  common_attr <- NULL

  # To original granularity
  to_mixtime <- function(x) {
    if(inherits(x, "mixtime")) return(x)

    attributes(x) <- common_attr

    # Less than perfect ('hack')
    #
    # The default coord limits are
    # `transformation$transform(transformation$inverse(c(NA_real_, NA_real_)))`,
    # which then also requires `Scale$transform$transform` to return atomic
    # numeric values :(
    #
    # A non-mixtime object is required here in order to have `ifelse()` work
    # correctly in `ggplot2:::expand_limits_continuous_trans()`
    #
    # This is also required for other aspects of ggplot2, such as for `pretty()`
    # to work in computing the default breaks.
    return(x)
  }
  # To common granularity (possibly with alignment?)
  # If local time is set, then an offset argument should be passed into the geom?
  from_mixtime <- function(x) {
    if(!inherits(x, "mixtime")) return(x)
    if (length(attr(x, "v")) != 1L) {
      cli::cli_abort("{.fun transform_mixtime} currently works with single granularity vectors only.")
    }
    common_attr <<- attributes(attr(x, "v")[[1L]])
    return(x)

    # structure(as.numeric(vecvec::unvecvec(x)), names = names(x), x = x)
  }
  scales::new_transform(
    "mixtime", transform = "from_mixtime", inverse = "to_mixtime",
    breaks = scales::breaks_pretty()#,
    #domain = to_mixtime(c(-Inf, Inf))
  )
}


clock_offset <- function(x) {
  tryCatch(
    clock::zoned_time_info(
      clock::as_zoned_time(x)
    )$offset,
    error = function(e) rep(0, length(x))
  )
}
tz_offset <- function(x) {
  if(!mixtime::is_mixtime(x)) {
    cli::cli_warn("Missing timezone offset could not be calculated in the scale.")
  }
  attr(x, "v") <- lapply(attr(x, "v"), clock_offset)
  as.numeric(vecvec::unvecvec(x))
}
