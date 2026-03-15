#' Plot Atomic Force Microscopy Data
#'
#' @description
#' Plots an atomic force microscopy (AFM) image or frequency spectrum.
#'
#' AFM images may contain multiple channels (for example, topography or phase).
#' Use `summary()` to inspect available channels, then select the desired channel
#' with `no`. Image contrast can be enhanced by trimming extreme high and low
#' values with `trimPeaks`. Alternatively, `setRange` can be used to explicitly
#' define the displayed color scale range. Values below the requested minimum are
#' clipped to the minimum color, and values above the requested maximum are
#' clipped to the maximum color.
#'
#' Several plot styles are available through `graphType`, including plain image,
#' image with legend, image with scale bar, and legend-only output. Since the
#' returned object is a `ggplot`, additional themes or layers can be added by the
#' user.
#'
#' @param x An `AFMdata` object.
#' @param no Integer channel number to plot.
#' @param mpt Numeric midpoint for `redBlue = TRUE`. If `NA`, the midpoint is set
#'   to the mean of the plotted `z` values.
#' @param graphType Integer plot style:
#'   \describe{
#'     \item{1}{Image with axes and legend outside.}
#'     \item{2}{Image with scale bar and legend inside.}
#'     \item{3}{Plain image without axes or legend.}
#'     \item{4}{Plain image with scale bar, without axes or legend.}
#'     \item{5}{Legend only.}
#'   }
#' @param trimPeaks Numeric value from 0 to 1 giving the fraction of extreme data
#'   values to trim from the histogram tails before plotting. Values less than
#'   `0.01` are typically useful.
#' @param fillOption Character viridis palette option passed to
#'   [ggplot2::scale_fill_viridis_c()], usually a letter from "A" through "H".
#' @param addLines Logical; if `TRUE`, stored line profiles are overlaid by
#'   assigning them the minimum color value.
#' @param redBlue Logical; if `TRUE`, use a red-white-blue diverging color scale
#'   instead of viridis.
#' @param verbose Logical; if `TRUE`, print additional diagnostic information.
#' @param quiet Logical; if `TRUE`, suppress normal console output.
#' @param setRange Numeric vector of length 2 giving the plotting range, for
#'   example `c(-30, 30)`. Values outside this range are clipped to the nearest
#'   endpoint before plotting. If `c(0, 0)`, the range is taken from the image
#'   data.
#' @param ... Additional arguments passed to `geom_line()` and `geom_text()` used
#'   for the scale bar, for example `color = "white"`.
#'
#' @returns A `ggplot` object.
#'
#' @author Thomas Gredig
#'
#' @importFrom ggplot2 ggplot aes geom_raster geom_line geom_text theme_bw
#' @importFrom ggplot2 scale_fill_gradient2 scale_fill_viridis_c
#' @importFrom ggplot2 xlab ylab labs scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 coord_equal theme element_rect element_blank
#' @importFrom ggpubr get_legend as_ggplot
#'
#' @method plot AFMdata
#' @export
plot.AFMdata <- function(
    x,
    no = 1,
    mpt = NA,
    graphType = 1,
    trimPeaks = 0.01,
    fillOption = "D",
    addLines = FALSE,
    redBlue = FALSE,
    verbose = FALSE,
    quiet = FALSE,
    setRange = c(0, 0),
    ...
) {
  validate_plot_inputs(x, no, quiet, verbose)
  
  if (AFM.dataType(x) == "frequency") {
    return(plot_frequency_data(x))
  }
  
  if (!(AFM.isImage(x) || AFM.dataType(x) == "spectroscopy")) {
    stop("Unsupported AFM data type.")
  }
  
  prep <- prepare_raster_data(
    x = x,
    no = no,
    trimPeaks = trimPeaks,
    addLines = addLines,
    verbose = verbose,
    setRange = setRange
  )
  
  d <- prep$d
  setRange <- prep$setRange
  
  z_lab <- make_z_label(x, no, graphType)
  
  if (is.na(mpt)) {
    mpt <- mean(d$z, na.rm = TRUE)
  }
  
  if (verbose) {
    cat(sprintf(
      "z range: %s to %s, midpoint: %s\n",
      min(d$z, na.rm = TRUE),
      max(d$z, na.rm = TRUE),
      mpt
    ))
  }
  
  fill_scale <- make_fill_scale(
    redBlue = redBlue,
    fillOption = fillOption,
    setRange = setRange,
    mpt = mpt
  )
  
  base_plot <- make_base_image_plot(
    d = d,
    fill_scale = fill_scale,
    z_lab = z_lab,
    show_axes = graphType %in% c(1, 5),
    show_legend = graphType %in% c(1, 2, 5)
  )
  
  g1 <- switch(
    as.character(graphType),
    "1" = base_plot + ggplot2::theme_bw(),
    "2" = add_scale_bar(
      p = base_plot,
      x = x,
      show_legend_inside = TRUE,
      ...
    ),
    "3" = strip_plot(base_plot),
    "4" = strip_plot(
      add_scale_bar(
        p = base_plot,
        x = x,
        show_legend_inside = FALSE,
        ...
      )
    ),
    "5" = legend_only_plot(base_plot),
    stop("graphType is not supported.")
  )
  
  g1
}

validate_plot_inputs <- function(x, no, quiet, verbose) {
  if (no > length(x@channel)) {
    stop("imageNo out of bounds.")
  }
  
  if (!quiet) {
    cat("Graphing:", x@channel[no], "\n")
  }
  
  if (verbose) {
    print(paste("History:", x@history))
  }
}

prepare_raster_data <- function(x, no, trimPeaks, addLines, verbose, setRange) {
  d <- AFM.raster(x, no)
  
  if (trimPeaks > 0) {
    d$z <- trim_z_peaks(x, no, d$z, trimPeaks)
  }
  
  if (addLines) {
    d$z <- apply_line_overlay(x, d$z, verbose)
  }
  
  range_info <- resolve_and_clip_range(d$z, setRange)
  d$z <- range_info$z
  
  list(
    d = d,
    setRange = range_info$setRange
  )
}

trim_z_peaks <- function(x, no, z, trimPeaks) {
  qHist <- AFM.histogram(x, no, dataOnly = TRUE)
  csHist <- cumsum(qHist$zDensity)
  csHist <- csHist / max(csHist)
  
  lower_idx <- tail(which(csHist < (trimPeaks / 2)), n = 1)
  upper_idx <- head(which(csHist > (1 - trimPeaks / 2)), n = 1)
  
  lower_bound <- qHist$mids[lower_idx]
  upper_bound <- qHist$mids[upper_idx]
  
  z[z < lower_bound] <- lower_bound
  z[z > upper_bound] <- upper_bound
  z
}

apply_line_overlay <- function(x, z, verbose) {
  if (is.null(x@data$line)) {
    warning("No lines attached.")
    return(z)
  }
  
  if (verbose) {
    cat("Adding lines using min. value for color.\n")
  }
  
  z_min <- min(z, na.rm = TRUE)
  
  for (zLine in x@data$line) {
    z[zLine] <- z_min
  }
  
  z
}

resolve_and_clip_range <- function(z, setRange) {
  if (length(setRange) != 2) {
    stop("setRange must be a numeric vector of length 2.")
  }
  
  if (setRange[1] == 0 && setRange[2] == 0) {
    setRange <- range(z, na.rm = TRUE)
  } else {
    z <- pmin(pmax(z, setRange[1]), setRange[2])
  }
  
  list(
    z = z,
    setRange = setRange
  )
}

make_z_label <- function(x, no, graphType) {
  if (graphType %in% c(2, 3, 4)) {
    return(x@z.units[no])
  }
  
  z_lab <- paste0(x@channel[no], " (", x@z.units[no], ")")
  gsub("Retrace|Trace", "", z_lab)
}

make_fill_scale <- function(redBlue, fillOption, setRange, mpt) {
  if (redBlue) {
    ggplot2::scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      midpoint = mpt,
      limits = setRange
    )
  } else {
    ggplot2::scale_fill_viridis_c(
      option = fillOption,
      limits = setRange
    )
  }
}

make_base_image_plot <- function(d, fill_scale, z_lab, show_axes = TRUE, show_legend = TRUE) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x / 1000, y / 1000, fill = z)) +
    ggplot2::geom_raster() +
    fill_scale +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_equal()
  
  if (show_axes) {
    p <- p +
      ggplot2::xlab(expression(paste("x (", mu, "m)"))) +
      ggplot2::ylab(expression(paste("y (", mu, "m)"))) +
      ggplot2::labs(fill = z_lab)
  } else {
    p <- p +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::labs(fill = z_lab)
  }
  
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  
  p
}

make_scale_bar_data <- function(obj) {
  bar_length <- signif(obj@x.nm * 0.2, 2)
  bar_x_start <- 0.05 * obj@x.pixels * obj@x.conv
  bar_y_start <- 0.05 * obj@y.pixels * obj@y.conv
  bar_x_end <- bar_x_start + bar_length
  
  data.frame(
    x = c(bar_x_start, bar_x_end),
    y = c(bar_y_start, bar_y_start),
    myLabel = c(paste(bar_length, "nm"), "")
  )
}

#' @importFrom rlang .data
add_scale_bar <- function(p, x, show_legend_inside = FALSE, ...) {
  d_line <- make_scale_bar_data(x)
  
  p <- p +
    ggplot2::geom_line(
      data = d_line,
      ggplot2::aes(x / 1000, y / 1000),
      linewidth = 4,
      inherit.aes = FALSE,
      ...
    ) +
    ggplot2::geom_text(
      data = d_line,
      ggplot2::aes( x / 1000, y / 1000,
        label =  myLabel
      ),
      vjust = -1,
      hjust = 0,
      inherit.aes = FALSE,
      ...
    ) +
    ggplot2::theme_bw()
  
  if (show_legend_inside) {
    p <- p + ggplot2::theme(
      legend.position.inside = c(0.99, 0.01),
      legend.justification = c(1, 0)
    )
  }
  
  p
}

strip_plot <- function(p) {
  p +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}

legend_only_plot <- function(p) {
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.box.background = ggplot2::element_rect(
        fill = "transparent",
        colour = "transparent"
      )
    )
  
  g1l <- ggpubr::get_legend(p)
  ggpubr::as_ggplot(g1l)
}

#' @importFrom rlang .data
plot_frequency_data <- function(x) {
  d <- AFM.raster(x)
  
  ggplot2::ggplot(d, ggplot2::aes(.data$freq.Hz / 1e3, .data$z.V)) +
    ggplot2::geom_line(col = "red", linewidth = 2) +
    ggplot2::xlab("f (kHz)") +
    ggplot2::ylab("V (V)") +
    ggplot2::theme_bw()
}