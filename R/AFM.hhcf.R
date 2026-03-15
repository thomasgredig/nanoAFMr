#' Height-height correlation function for AFM image
#'
#' @description
#' Computes the height-height correlation function (HHCF) for an `AFMdata`
#' object. Background removal should generally be performed first.
#'
#' To reduce computation time, the HHCF is estimated from a random subset of
#' point pairs rather than from all possible pixel pairs. For each iteration,
#' a random starting pixel and random angle are selected, and the quantity
#' \eqn{g(r) = |h(x) - h(x+r)|^2} is evaluated for distances from 1 pixel up to
#' the maximum distance determined by `r.percentage`.
#'
#' Since AFM images are usually square, some point-angle combinations do not
#' remain inside the image for large \eqn{r}; those cases are ignored.
#' Results may vary slightly between runs unless `randomSeed` is set.
#'
#' If `addFit = TRUE`, the HHCF data are fit to
#' \deqn{2 \sigma^2 \left(1 - \exp\left[-\left(\frac{r}{\xi}\right)^{2H}\right]\right)}
#'
#' where:
#' \itemize{
#'   \item \eqn{\sigma} is the roughness
#'   \item \eqn{\xi} is the correlation length
#'   \item \eqn{H} is the Hurst parameter
#' }
#'
#' Reference:
#' Thomas Gredig, Evan A. Silverstein, Matthew P. Byrne,
#' \emph{Height-Height Correlation Function to Determine Grain Size in Iron
#' Phthalocyanine Thin Films},
#' Journal of Physics: Conference Series 417, 012069 (2013).
#'
#' @param obj An `AFMdata` object.
#' @param no Integer channel number.
#' @param numIterations Number of random iterations. Must be at least 1000.
#' @param addFit Logical; if `TRUE`, fit the HHCF model to the computed data.
#' @param dataOnly Logical; if `TRUE`, return only the HHCF data frame.
#'   Deprecated in spirit; prefer `allResults = TRUE`.
#' @param degRes Angular resolution factor. Larger values give finer angular
#'   sampling.
#' @param r.percentage Maximum distance as a percentage of image width.
#' @param xi.percentage Percentage of the asymptotic amplitude used to estimate
#'   the initial guess for correlation length.
#' @param randomSeed Optional integer seed for reproducibility.
#' @param allResults Logical; if `TRUE`, return a list with graph, data, fit
#'   curve, and fit parameters.
#' @param addFitLabels Logical; if `TRUE` and `addFit = TRUE`, annotate the fit
#'   parameters on the graph.
#' @param verbose Logical; if `TRUE`, print progress and timing information.
#'
#' @returns If `dataOnly = TRUE` and `allResults = FALSE`, a data frame with
#'   columns `r.nm`, `g`, and `num`. Otherwise returns a `ggplot` object, or a
#'   list containing `graph`, `data`, `fitData`, and `fitParams` if
#'   `allResults = TRUE`.
#'   
#' @author Thomas Gredig
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_x_log10 scale_y_log10
#' @importFrom ggplot2 theme_bw geom_label theme xlab ylab
#' @importFrom stats runif nls predict coef
#' @export
AFM.hhcf <- function(
    obj,
    no = 1,
    numIterations = 10000,
    addFit = TRUE,
    dataOnly = FALSE,
    degRes = 100,
    r.percentage = 80,
    xi.percentage = 70,
    randomSeed = NA,
    allResults = FALSE,
    addFitLabels = TRUE,
    verbose = FALSE
) {
  myLabel <- NULL
  
  if (!is(obj, "AFMdata")) {
    stop("obj must be an AFMdata object.")
  }
  
  if (no < 1 || no > length(obj@data$z)) {
    stop("Channel number 'no' is out of bounds.")
  }
  
  if (obj@x.conv != obj@y.conv) {
    warning("AFM image is distorted in x- and y-direction; HHCF may be incorrect.")
  }
  
  dimx <- obj@x.pixels
  dimy <- obj@y.pixels
  q <- obj@data$z[[no]]
  
  if (verbose) {
    cat("AFM image is", dimx, "by", dimy, "pixels.\n")
  }
  
  if (numIterations < 1000) {
    numIterations <- 1000
    if (verbose) {
      cat("numIterations must be at least 1000; reset to 1000.\n")
    }
  }
  
  if (!is.na(randomSeed)) {
    set.seed(randomSeed)
  }
  
  px1 <- round(runif(numIterations, min = 1, max = dimx))
  py1 <- round(runif(numIterations, min = 1, max = dimy))
  theta <- round(runif(numIterations, min = 0, max = 2 * pi * degRes)) / degRes
  
  maxR <- round(dimx * r.percentage / 100)
  
  if (verbose) {
    cat(
      "Computing r from 1 pixel to", maxR,
      "pixels maximum. Adjust with r.percentage parameter.\n"
    )
  }
  
  g_values <- numeric(maxR)
  n_values <- integer(maxR)
  
  t_start <- Sys.time()
  
  for (r_idx in seq_len(maxR)) {
    px2 <- round(px1 + r_idx * cos(theta))
    py2 <- round(py1 + r_idx * sin(theta))
    
    keep <- which(px2 > 0 & px2 <= dimx & py2 > 0 & py2 <= dimy)
    
    if (length(keep) == 0L) {
      g_values[r_idx] <- NA_real_
      n_values[r_idx] <- 0L
      next
    }
    
    p1 <- px1[keep] + (py1[keep] - 1) * dimx
    p2 <- px2[keep] + (py2[keep] - 1) * dimx
    
    g_values[r_idx] <- mean((q[p1] - q[p2])^2)
    n_values[r_idx] <- length(keep)
  }
  
  t_end <- Sys.time()
  
  if (verbose) {
    cat("Time used:", round(as.numeric(t_end - t_start), 1), "seconds\n")
  }
  
  hhcf_data <- data.frame(
    r.nm = seq_len(maxR) * obj@x.conv,
    g = g_values,
    num = n_values
  )
  
  results <- list(data = hhcf_data)
  
  if (dataOnly && !allResults) {
    return(hhcf_data)
  }
  
  fit <- NULL
  fit_curve <- NULL
  fit_params <- NULL
  fit_labels <- NULL
  
  if (addFit) {
    fit_info <- fit_hhcf_curve(
      obj = obj,
      hhcf_data = hhcf_data,
      xi.percentage = xi.percentage,
      verbose = verbose
    )
    
    fit <- fit_info$fit
    
    if (!is.null(fit)) {
      fit_curve <- fit_info$fitData
      fit_params <- fit_info$fitParams
      fit_labels <- fit_info$fitLabels
      
      results$fitData <- fit_curve
      results$fitParams <- fit_params
    } else {
      addFit <- FALSE
    }
  }
  
  graph <- plot_hhcf_data(
    hhcf_data = hhcf_data,
    fit_curve = fit_curve,
    fit_labels = fit_labels,
    addFit = addFit,
    addFitLabels = addFitLabels
  )
  
  results$graph <- graph
  
  if (allResults) {
    return(results)
  }
  
  graph
}

fit_hhcf_curve <- function(obj, hhcf_data, xi.percentage, verbose = FALSE) {
  valid <- is.finite(hhcf_data$g) & hhcf_data$g > 0
  d <- hhcf_data[valid, , drop = FALSE]
  
  if (nrow(d) < 5) {
    if (verbose) {
      cat("Not enough valid HHCF data points for fitting.\n")
    }
    return(list(fit = NULL))
  }
  
  params <- AFM.math.params(obj)
  sigma_guess <- params$Rq
  
  xi_threshold <- (xi.percentage * sigma_guess^2 * 2) / 100
  xi_idx <- which(d$g > xi_threshold)[1]
  
  if (is.na(xi_idx)) {
    xi_guess <- stats::median(d$r.nm)
  } else {
    xi_guess <- d$r.nm[xi_idx]
  }
  
  fit <- tryCatch(
    stats::nls(
      g ~ 2 * sigma^2 * (1 - exp(-(r.nm / xi)^(2 * H))),
      data = d,
      start = list(sigma = sigma_guess, xi = xi_guess, H = 1)
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    if (verbose) {
      cat("Cannot fit data; fit disabled.\n")
    }
    return(list(fit = NULL))
  }
  
  if (verbose) {
    cat("Fit is successful.\n")
  }
  
  fit_r <- seq(
    from = round(min(d$r.nm) * 0.9),
    to = max(d$r.nm),
    by = 1
  )
  
  fit_data <- data.frame(
    r.nm = fit_r,
    g = stats::predict(fit, list(r.nm = fit_r))
  )
  
  coef_vec <- stats::coef(fit)
  coef_table <- summary(fit)$coef
  
  fit_params <- data.frame(
    sigma = coef_table["sigma", "Estimate"],
    xi = coef_table["xi", "Estimate"],
    Hurst = coef_table["H", "Estimate"],
    sigma.err = coef_table["sigma", "Std. Error"],
    xi.err = coef_table["xi", "Std. Error"],
    Hurst.err = coef_table["H", "Std. Error"]
  )
  
  fit_labels <- data.frame(
    r.nm = d$r.nm[seq_len(min(3, nrow(d)))],
    g = d$g[seq_len(min(3, nrow(d)))],
    myLabel = c(
      paste("sigma =", signif(coef_vec["sigma"], 4), "nm"),
      paste("xi =", signif(coef_vec["xi"], 4), "nm"),
      paste("H =", signif(coef_vec["H"], 4))
    )[seq_len(min(3, nrow(d)))]
  )
  #print(fit_labels)
  
  list(
    fit = fit,
    fitData = fit_data,
    fitParams = fit_params,
    fitLabels = fit_labels
  )
}

plot_hhcf_data <- function(hhcf_data, fit_curve = NULL, fit_labels = NULL,
                           addFit = TRUE, addFitLabels = TRUE) {
  g <- ggplot2::ggplot(hhcf_data, ggplot2::aes(r.nm, g)) +
    ggplot2::geom_point(col = "blue", size = 2) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::ylab("g(r)") +
    ggplot2::xlab("r (nm)") +
    ggplot2::theme_bw()
  
  if (addFit && !is.null(fit_curve)) {
    g <- g +
      ggplot2::geom_path(data = fit_curve, col = "red") +
      ggplot2::theme(legend.position = "none")
  }
  
  if (addFit && addFitLabels && !is.null(fit_labels)) {
    g <- g +
      ggplot2::geom_label(
        data = fit_labels,
        aes(x=r.nm, y=g, label = myLabel),
        fill = "white",
        colour = "black",
        fontface = "bold",
        hjust = -0.1,
        inherit.aes = FALSE
      )
  }
  
  g
}