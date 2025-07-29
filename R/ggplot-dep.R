set_sec_axis <- function(sec.axis, scale) {
  if (!is_waiver(sec.axis)) {
    if (scale$is_discrete()) {
      if (!identical(.subset2(sec.axis, "trans"), identity)) {
        cli::cli_abort("Discrete secondary axes must have the {.fn identity} transformation.")
      }
    }
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) {
      cli::cli_abort("Secondary axes must be specified using {.fn sec_axis}.")
    }
    scale$secondary.axis <- ggplot2::sec.axis
  }
  return(scale)
}

# ggplot2:::ggplot_global
ggplot_global <- list(
  x_aes = c("x", "xmin", "xmax", "xend", "xintercept",
            "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
  y_aes = c("y", "ymin", "ymax", "yend", "yintercept",
            "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")
)

gg_par <- function(..., stroke = NULL, pointsize = NULL) {
  args <- rlang::list2(...)
  args <- args[lengths(args) > 0]

  if (!is.null(args$lwd)) {
    args$lwd <- args$lwd * .pt
  }
  if (!is.null(stroke)) {
    args$lwd <- stroke * .stroke / 2
  }
  if (!is.null(pointsize)) {
    # Stroke is added around the outside of the point
    stroke <- stroke %||% 0
    stroke[is.na(stroke)] <- 0
    args$fontsize <- pointsize * .pt + stroke * .stroke / 2
  }
  if (!is.null(args$lty) && anyNA(args$lty)) {
    args$lty[is.na(args$lty)] <- if (is.character(args$lty)) "blank" else 0
  }

  rlang::inject(gpar(!!!args))
}

is_waiver <- function(x) inherits(x, "waiver")
