#' ggradar
#'
#' @param plot.data dataframe comprising one row per group
#' @param base.size text size
#' @param font.radar text font family
#' @param values.radar values to print at minimum, 'average', and maximum gridlines
#' @param start.degree starting point of the plot, i.e. degree of rotation. Similar to the 'start' variable in coord_polar() in ggplot2.
#' @param axis.labels  names of axis labels if other than column names supplied via plot.data
#' @param grid.min value at which mininum grid line is plotted
#' @param grid.mid value at which 'average' grid line is plotted
#' @param grid.max value at which maximum grid line is plotted
#' @param centre.y value of y at centre of plot
#' @param plot.extent.x.sf controls relative size of plot horizontally
#' @param plot.extent.y.sf controls relative size of plot vertically
#' @param x.centre.range controls axis label alignment
#' @param label.centre.y whether value of y at centre of plot should be labelled
#' @param grid.line.width width of gridline
#' @param gridline.min.linetype line type of minimum gridline
#' @param gridline.mid.linetype line type of 'average' gridline
#' @param gridline.max.linetype line type of maximum gridline
#' @param gridline.min.colour colour of minimum gridline
#' @param gridline.mid.colour colour of 'average' gridline
#' @param gridline.max.colour colour of maximum gridline
#' @param grid.label.size text size of gridline label
#' @param gridline.label.offset displacement to left/right of central vertical axis
#' @param label.gridline.min whether or not to label the mininum gridline
#' @param label.gridline.mid whether or not to label the 'mininum'average' gridline
#' @param label.gridline.max whether or not to label the maximum gridline
#' @param axis.label.offset vertical displacement of axis labels from maximum grid line, measured relative to circle diameter
#' @param axis.label.size text size of axis label
#' @param axis.line.colour colour of axis line
#' @param group.line.width line width of group
#' @param group.point.size point size of group
#' @param group.colours colour of group
#' @param background.circle.colour colour of background circle/radar
#' @param background.circle.transparency transparency of background circle/radar
#' @param plot.legend whether to include a plot legend
#' @param legend.title title of legend
#' @param plot.title title of radar plot
#' @param legend.text.size text size in legend
#' @param legend.position position of legend, valid values are "top", "right", "bottom", "left"
#' @param fill whether to fill polygons
#' @param fill.alpha if filling polygons, transparency values
#'
#' @import ggplot2
#' @import ggnewscale
#' @return a ggplot object
#'
#' @name ggradar-package
#'
#' @export
#'
#' @source
#' Most of the code is from \url{http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html}.
#'
#' @examples
#' \dontrun{
#' library(ggradar)
#' library(dplyr)
#' library(scales)
#' library(tibble)
#'
#' mtcars_radar <- mtcars %>%
#'   as_tibble(rownames = "group") %>%
#'   mutate_at(vars(-group), rescale) %>%
#'   tail(4) %>%
#'   select(1:10)
#' mtcars_radar
#' ggradar(mtcars_radar)
#' }
#' 
#' 
#' # install.packages("remotes")
#' remotes::install_github("AllanCameron/geomtextpath")
#' 
#' 
ggradar <- function(plot.data,
                    base.size = 15,
                    font.radar = "sans",
                    start.degree = 0,
                    axis.labels = colnames(plot.data)[-1],
                    centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.max - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    grid.min = gridline[1],
                    grid.max = gridline[length(gridline)],
                    gridline = 0, 
                    gridline.label = TRUE,
                    girdline.labels = as.character(gridline),
                    gridline.linetype = "longdash",
                    gridline.colour = "grey",
                    grid.label.size = 6,
                    gridline.label.offset = -0.1 * (grid.max - centre.y),
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = "grey",
                    group.line.width = 1.5,
                    group.point.size = 6,
                    group.colours = NULL,
                    group.cluster = NULL,
                    group.cluster.lwt = 3,
                    group.cluster.alpha = 1,
                    group.cluster.relRad = 1.3,
                    group.cluster.colours = NULL,
                    background.circle.colour = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title = "",
                    plot.title = "",
                    legend.text.size = 14,
                    legend.position = "left",
                    fill = FALSE,
                    fill.alpha = 0.5,
                    theme_black = FALSE) {
  plot.data <- as.data.frame(plot.data)

  if(!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
  }

  var.names <- colnames(plot.data)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n

  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf

  # Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }
  if (max(plot.data[, -1]) > grid.max) {
    stop("'plot.data' contains value(s) > grid.max", call. = FALSE)
  }

  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
  # print(plot.data.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset, start = start.degree)

  # print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y), start = start.degree)
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  startRadians <- start.degree * (pi/180)
  angles <- seq(from = 0 + startRadians, to = (2 * pi) + startRadians, by = (2 * pi) / n.vars) %% (2 * pi)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  

  # print(gridline$min$label)
  # print(gridline$max$label)
  # print(gridline$mid$label)
  
  ### Group Clustering
  if (!is.null(group.cluster)){
    if(length(group.cluster) != (n.vars)){
      stop(paste("'group.cluster' is not the same length as the number of variables in the model.", length(group.cluster), "/", n.vars), call. = FALSE)
    }
    group.cluster.length <- length(group.cluster)
    group.cluster.granularity <- 25 
    #group.cluster.disconnect <- group.cluster != c(group.cluster[-1], group.cluster[1])
    group.cluster.disconnect <- group.cluster != c(group.cluster[length(group.cluster)], group.cluster[-length(group.cluster)])

    group.cluster.fullCircle <- apply(funcCircleCoords(c(0, 0),
                                                 grid.max + abs(centre.y) * group.cluster.relRad,
                                                 npoints = group.cluster.length * group.cluster.granularity,
                                                 start = 90 - start.degree + (360 / group.cluster.length  /2)
                                                 ), 2, rev)
    group.cluster.segmentCircle <- split(as.data.frame(group.cluster.fullCircle), rep(cumsum(group.cluster.disconnect) +1, each = group.cluster.granularity))
    group.cluster.overlap0 <- group.cluster[1] == group.cluster[length(group.cluster)]
    if(group.cluster.overlap0){
      group.cluster.segmentCircle <- c(list(rbind.data.frame(group.cluster.segmentCircle[[length(group.cluster.segmentCircle)]],
                                                             group.cluster.segmentCircle[[1]])),
                                            group.cluster.segmentCircle[-c(1, length(group.cluster.segmentCircle))])
    }
    # group.cluster.segmentCircle <- c(list(group.cluster.segmentCircle[[1]][-nrow(group.cluster.segmentCircle[[1]]), ]),
    #                                  lapply(group.cluster.segmentCircle[-1], function(m){return(m[-1, ])}))
    group.cluster.segmentCircle <- c(list(group.cluster.segmentCircle[[1]][-c(1, 2), ]),
                                     lapply(group.cluster.segmentCircle[-1], function(m){return(m[-1, ])}))
    if(is.null(group.cluster.colours)){
      group.cluster.colours <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], length(unique(group.cluster)))
      group.cluster.coloursSegments <- as.character(factor(group.cluster[group.cluster.disconnect], labels=group.cluster.colours))
    } else {
      group.cluster.coloursSegments <- group.cluster.colours
    }
    
    group.cluster.labels <- group.cluster[group.cluster.disconnect]
    str(group.cluster.segmentCircle)
  }
  
  
  
  
  
  ### Start building up the radar plot

  # Declare 'theme_clear', with or without a plot legend as required by user
  # [default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size = base.size) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.key = element_rect(linetype = "blank")
    )

  if (plot.legend == FALSE) legend.position = "none"

  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]
  
  
  
  
  
  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal()
    # + group clusters
    if(!is.null(group.cluster)){
      for(i in seq_along(group.cluster.segmentCircle)){
        base <- base + geom_path(
          data = group.cluster.segmentCircle[[i]], aes(x = x, y = y),
          lty = 1, colour = group.cluster.coloursSegments[i], size = group.cluster.lwt,
          alpha = group.cluster.alpha
        )
        
        base <- base + geom_textpath(
          data = group.cluster.segmentCircle[[i]],
          aes(x = x, y = y),
          label = group.cluster.labels[i],
          size = group.cluster.lwt,
          text_only = TRUE,
          fontface = "bold"
        )
      }
    }
    base <- base + 
    geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar, colour = ifelse(theme_black, "white", "black")
    ) +
    scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))

  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  # base <- base + geom_path(
  #   data = gridline$min$path, aes(x = x, y = y),
  #   lty = gridline.min.linetype, colour = gridline.min.colour, size = grid.line.width
  # )
  # base <- base + geom_path(
  #   data = gridline$mid$path, aes(x = x, y = y),
  #   lty = gridline.mid.linetype, colour = gridline.mid.colour, size = grid.line.width
  # )
  # base <- base + geom_path(
  #   data = gridline$max$path, aes(x = x, y = y),
  #   lty = gridline.max.linetype, colour = gridline.max.colour, size = grid.line.width
  # )
  for(i in seq_along(gridline)){
    line <- gridline[i]
    curGridline <- funcCircleCoords(c(0, 0), line + abs(centre.y), npoints = 360)
    
    # gridline labels
    base <- base + geom_path(
      data = curGridline, aes(x = x, y = y),
      lty = gridline.linetype[min(c(i, length(gridline.linetype)))], colour = gridline.colour[min(c(i, length(gridline.colour)))], size = grid.line.width
    )

  }
  
  
    # base <- base + geom_blank(aes(group = group.cluster,
    #                               colour = as.character(factor(group.cluster, labels=group.cluster.coloursSegments))))
    # base <- base + scale_fill_manual(name="Clustering",
    #                                     values  = unique(group.cluster.coloursSegments),
    #                                     labels = unique(group.cluster),
    #                                     drop = FALSE)
    #base <- base + ggnewscale::new_scale_colour()
    
  

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0.5, family = font.radar, colour = ifelse(theme_black, "white", "black")
  )
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, axis$label$x > x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0, family = font.radar, colour = ifelse(theme_black, "white", "black")
  )
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(
    data = curGridline, aes(x, y),
    fill = ifelse(theme_black, "black", background.circle.colour),
    alpha = background.circle.transparency
  )

  # + radial axes
  base <- base + geom_path(
    data = axis$path, aes(x = x, y = y, group = axis.no),
    colour = axis.line.colour
  )

  theGroupName <- names(group$path[1])

  # ... + group (cluster) 'paths'
  base <- base + geom_path(
    data = group$path, aes_string(x = "x", y = "y", group = theGroupName, colour = theGroupName),
    size = group.line.width
  )

  # ... + group points (cluster data)
  base <- base + geom_point(data = group$path, aes_string(x = "x", y = "y", group = theGroupName, colour = theGroupName), size = group.point.size)

  # ... + group (cluster) fills
  if(fill == TRUE) {
    base <- base + geom_polygon(data = group$path, aes_string(x = "x", y = "y", group = theGroupName, fill = theGroupName), alpha = fill.alpha)
  }


  # ... + amend Legend title
  if (plot.legend == TRUE) base <- base + labs(colour = legend.title, size = legend.text.size)

  # # ... + grid-line labels (max; mid; min)
  # if (label.gridline.min == TRUE) {
  #   base <- base + geom_text(aes(x = x, y = y, label = values.radar[1]), data = gridline$min$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar, colour = ifelse(theme_black, "white", "black"))
  # }
  # if (label.gridline.mid == TRUE) {
  #   base <- base + geom_text(aes(x = x, y = y, label = values.radar[2]), data = gridline$mid$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar, colour = ifelse(theme_black, "white", "black"))
  # }
  
  for(i in seq_along(gridline)){
    line <- gridline[i]
    curLabelPos <- data.frame(
      x = gridline.label.offset, y = line + abs(centre.y),
      text = as.character(line)
    )
    print(str(curLabelPos))
    base <- base + geom_text(aes(x = x, y = y),
                             label = girdline.labels[min(i, length(girdline.labels))],
                             data = curLabelPos,
                             size = grid.label.size * 0.8,
                             hjust = 1,
                             family = font.radar,
                             colour = ifelse(theme_black, "white", "black"))
  }
  

  
  
  
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text), data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar, colour = ifelse(theme_black, "white", "black"))
  }

  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, 100)
  } else {
    colour_values <- rep(c(
      "#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051",
      "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"
    ), 100)
  }

  base <- base + theme(legend.key.width = unit(3, "line")) + theme(text = element_text(
    size = 20,
    family = font.radar
  )) +
    theme(legend.text = element_text(size = legend.text.size), legend.position = legend.position) +
    theme(legend.key.height = unit(2, "line")) +
    scale_colour_manual(values = colour_values) +
    theme(text = element_text(family = font.radar)) +
    theme(legend.title = element_blank())


  if(isTRUE(fill)) {
    base <- base +
      scale_fill_manual(values = colour_values, guide = "none")
  }

  if(legend.title != "") {
    base <- base + theme(legend.title = element_text())
  }

  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  
  if(theme_black){
    base <- base + #theme_dark() + 
      theme(
        plot.background = element_rect(color = "black", fill = "black"),
        panel.background = element_rect(fill = "black", color  =  "black"),
        strip.background = element_rect(fill = "grey30", color = "grey10"),
        strip.text.x = element_text(color = "white"),
        strip.text.y = element_text(
          color = "white",
          angle = -90
        ),
        legend.text=element_text(color="white"),
        legend.title=element_text(color="white"),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        legend.key=element_blank(),
        text = element_text(colour = "white")
      ) 
  }

  return(base)
}
