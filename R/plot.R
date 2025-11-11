#' Graph Atomic Force Microscopy Image
#'
#' @description
#' Graphs an atomic force microscopy image with or without legends depending on the graphing type.
#' An AFM image could include multiple channels (topography, phase, etc.), use \code{summary} to 
#' view all channels and the submit the number of the channel. Generally, the image peaks and valleys
#' are removed to increas contrast; for example a trimPeak of 0.01 would remove 1 percent of the
#' height data points as determined from the histogram. Additionally, the fillOption and redBlue
#' can add different colors. Since the returning object is a ggplot graph, it is possible to add
#' themes and additional aesthetics. 
#' 
#'
#' @param x AFMdata object
#' @param no channel number of the image
#' @param mpt midpoint for coloring
#' @param graphType 1 = graph with legend outside, 2 = square graph with line bar, 3 = plain graph, 4 = plain graph with scale, 5 = legend only
#' @param trimPeaks value from 0 to 1, where 0=trim 0\% and 1=trim 100\% of data points, generally a value less than 0.01 is useful to elevate the contrast of the image
#' @param addLines if \code{TRUE} lines from obj are added to graph, lines can be added with  AFM.lineProfile for example
#' @param redBlue if \code{TRUE} output red / blue color scheme
#' @param fillOption can be one of 8 color palettes, use "A" ... "H", see scale_fill_viridis
#' @param setRange vector with two values, such as c(-30,30) to make the scale from -30 to +30
#' @param verbose if \code{TRUE} it outputs additional information.
#' @param quiet if \code{TRUE} then no output at all
#' @param ... other arguments, such as col='white' to change color of bar
#'
#' @returns ggplot graph
#'
#' @author Thomas Gredig
#'
#' @importFrom utils head tail
#' @importFrom ggplot2 ggplot aes geom_raster geom_line theme_bw scale_fill_gradient2 xlab ylab labs scale_y_continuous scale_x_continuous coord_equal geom_text theme element_rect element_blank
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom viridis scale_fill_viridis
#' @importFrom ggpubr get_legend as_ggplot
#' 
#' @method plot AFMdata
#' @export
plot.AFMdata <- function(x, no=1, mpt=NA, graphType=1, trimPeaks=0.01, fillOption='viridis',
                         addLines=FALSE, redBlue = FALSE, 
                         verbose=FALSE, quiet=FALSE, setRange = c(0,0), ...) {
  
  y <- z <- myLabel <- freq.Hz <- z.V <- NULL
  
  # check parameters
  if (no>length(x@channel)) stop("imageNo out of bounds.")
  if (!quiet) cat("Graphing:",x@channel[no])
  if (verbose) print(paste("History:",x@history))
  
  if (AFM.isImage(x) || AFM.dataType(x)=='spectroscopy') {
    d = AFM.raster(x,no)
    zLab = paste0(x@channel[no],' (',x@z.units[no],')')
    zLab = gsub('Retrace|Trace','',zLab)
    
    xlab <- expression(paste('x (',mu,'m)'))
    
    if (trimPeaks>0) {
      AFM.histogram(x, no, dataOnly = TRUE) -> qHist
      cumsum(qHist$zDensity) -> csHist
      csHist / max(csHist) -> csHist
      lowerBound = qHist$mids[tail(which(csHist<(trimPeaks/2)),n=1)]
      upperBound = qHist$mids[head(which(csHist>(1-trimPeaks/2)),n=1)]
      d$z[which(d$z<lowerBound)] <- lowerBound
      d$z[which(d$z>upperBound)] <- upperBound
    }
    
    # bit of a hack to set the range
    if (setRange[1]==0 & setRange[2]==0) {
      setRange = c(min(d$z), max(d$z))
      # d$z[1]=setRange[1]
      # d$z[2]=setRange[2]
    }
    
    if (addLines) {
      # check if there are lines
      if (is.null(x@data$line)) { warning("No lines attached.") }
      else {
        if (verbose) cat("Adding lines using min. value for color.\n")
        for(zLine in x@data$line) {
          d$z[zLine] = min(d$z)
        }
      }
    }
    
    if (is.na(mpt)) mean(d$z) -> mpt
    
    if (verbose) cat(paste("z range: ",min(d$z)," to ",max(d$z)," midpoint",mpt))
    sFill = scale_fill_viridis(option=fillOption, limits = setRange)
    if (redBlue) {
      sFill = scale_fill_gradient2(low='red', mid='white', high='blue', midpoint=mpt,limits = setRange) 
    } 
    
    if (graphType==1) {
      g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
        geom_raster() +
        sFill +
        xlab(xlab) +
        ylab(expression(paste('y (',mu,'m)'))) +
        labs(fill=zLab) +
        scale_y_continuous(expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        coord_equal() +
        theme_bw()
    } else if (graphType==2) {
      # figure out coordinates for line
      bar.length = signif(x@x.nm*0.2,2)  # nm
      bar.x.start = 0.05*x@x.pixels * x@x.conv
      bar.y.start = 0.05*x@y.pixels * x@y.conv
      bar.x.end = bar.x.start + bar.length
      d.line = data.frame(
        x = c(bar.x.start, bar.x.end),
        y = c(bar.y.start, bar.y.start),
        z = 1,
        myLabel = c(paste(bar.length,"nm"),"")
      )
      zLab = x@z.units[no]
      g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
        geom_raster() +
        sFill +
        xlab("") +
        ylab("") +
        labs(fill=zLab) +
        scale_y_continuous(expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        coord_equal() +
        geom_line(data = d.line, aes(x/1000,y/1000), linewidth=4, inherit.aes=FALSE, ...) +
        geom_text(data = d.line, aes(x/1000,y/1000,label=myLabel), vjust=-1, hjust=0, inherit.aes=FALSE, ...) +
        theme_bw() +
        theme(legend.position.inside =c(0.99,0.01),
              legend.justification = c(1,0)) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (graphType==3) {
      zLab = x@z.units[no]
      g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
        geom_raster() +
        sFill +
        xlab("") +
        ylab("") +
        scale_y_continuous(expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        coord_equal() +
        theme_bw() +
        theme(legend.position  = "none") +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (graphType==4) { # plain style with scale
      # figure out coordinates for line
      bar.length = signif(x@x.nm*0.2,2)  # nm
      bar.x.start = 0.05*x@x.pixels * x@x.conv
      bar.y.start = 0.05*x@y.pixels * x@y.conv
      bar.x.end = bar.x.start + bar.length
      d.line = data.frame(
        x = c(bar.x.start, bar.x.end),
        y = c(bar.y.start, bar.y.start),
        z = 1,
        myLabel = c(paste(bar.length,"nm"),"")
      )
      zLab = x@z.units[no]
      g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
        geom_raster() +
        sFill +
        xlab("") +
        ylab("") +
        labs(fill=zLab) +
        scale_y_continuous(expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        coord_equal() +
        geom_line(data = d.line, aes(x/1000,y/1000), linewidth=4, inherit.aes=FALSE, ...) +
        geom_text(data = d.line, aes(x/1000,y/1000,label=myLabel), vjust=-1, hjust=0, inherit.aes=FALSE, ...) +
        theme_bw() +
        theme(legend.position  = "none") +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (graphType==5) {
      # only display scales without the image
      g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
        geom_raster() +
        sFill +
        xlab(xlab) +
        ylab(expression(paste('y (',mu,'m)'))) +
        labs(fill=zLab) +
        scale_y_continuous(expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        coord_equal() +
        theme_bw() +
        theme(legend.box.background=element_rect(fill='transparent',
                                                 colour='transparent'))
      ggpubr::get_legend(g1) -> g1l
      g1 <- as_ggplot(g1l)
    } else stop('graphType is not supported.')
  } else if (AFM.dataType(x) == 'frequency') {
    ## graph the frequency
    d = AFM.raster(x)
    g1 <- ggplot(d, aes(freq.Hz/1e3, z.V)) +
      geom_line(col='red', size=2) +
      xlab('f (kHz)') +
      ylab('V (V)') +
      theme_bw()
  }
  
  g1
}