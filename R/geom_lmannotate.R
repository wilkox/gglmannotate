#' Add an annotation describing a linear model to a ggplot2 plot
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2
#'
#' @export
geom_lmannotate <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomLmAnnotate, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomLmAnnotate
#' @noRd
#' @import grid
#' @import ggfittext
GeomLmAnnotate <- ggplot2::ggproto("GeomLmAnnotate", ggplot2::Geom,
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    colour = NA, fill = "grey20", size = 0.5,
    linetype = 1, alpha = 1
  ),

  draw_key = ggplot2::draw_key_blank,

  setup_data = function(data, params) {

    # To optimise automatic placement of the annotations, it is useful to know
    # two things: the number of groups to be drawn in each panel, and the
    # ordered position of a given group within a panel. These will be
    # calculated and stored as data variables 'group_n' and 'group_i'
    panels <- split(data, data$PANEL)
    panels <- lapply(panels, function(panel) {
      groups <- unique(panel$group)
      group_n <- length(groups)
      panel$group_n <- rep(group_n, nrow(panel))
      panel$group_i <- match(panel$group, sort(groups))
      panel
    })
    data <- do.call(rbind, panels)
    data
  },

  draw_group = function(data, panel_params, coord) {

    # Must have at least two points to draw a line
    if (nrow(data) < 2) return(grid::nullGrob())

    coords <- coord$transform(data, panel_params)

    # Fit the lm and extract parameters with broom
    glance <- broom::glance(lm(y ~ x, data))
    tidy <- broom::tidy(lm(y ~ x, data))

    # Build a description of the model
    label <- stringr::str_c(
      "Adj.\u00A0R\u00B2\u00A0=\u00A0",
      signif(glance$adj.r.squared, 2),
      ", intercept\u00A0=\u00A0",
      signif(tidy$estimate[1], 2),
      ", slope\u00A0=\u00A0",
      stringr::str_replace(signif(tidy$estimate[2], 2), "-", "\u2212"),
      ", p\u00A0=\u00A0",
      signif(glance$p.value, 2)
    )

    # Set up the data frame to pass on to makecontent.fittexttree
    t_data <- data[1, ]

    # Set the graphical parameters for the text
    t_data$label <- label
    t_data$colour <- ifelse(is.na(data$colour[1]), "black", data$colour[1])
    t_data$angle <- 0
    t_data$size <- 10

    # Set the text placement and resizing parameters
    padding.x <- grid::unit(1, "mm")
    padding.y <- grid::unit(1, "mm")
    place <- "topright"
    min.size <- 0
    grow <- FALSE
    reflow <- FALSE
    height <- NULL

    # These parameters define the text drawing area for all annotations to be
    # drawn on a panel
    p_xmin <- 0
    p_xmax <- 1
    p_ymin <- 0.5
    p_ymax <- 1

    # Set the x limits for this annotation's drawing area
    t_data$xmin <- p_xmin
    t_data$xmax <- p_xmax

    # To set the y limits for this annotation's drawing area, divide the y
    # dimension of the panel's text drawing area into n horizontal slices where
    # n is the number of annotations to be drawn in that panel. This annotation
    # will be assigned the ith slice
    message("group n is ", data$group_n[1])
    slice_y_height <- (p_ymax - p_ymin) / data$group_n[1]
    message("slice_y_height is ", slice_y_height)
    t_data$ymax <- p_ymax - ((data$group_i[1] - 1) * slice_y_height)
    t_data$ymin <- p_ymax - (data$group_i[1] * slice_y_height)

    # Use ggfittext's fittexttree to draw the text
    gt <- grid::gTree(
      data = t_data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      min.size = min.size,
      grow = grow,
      reflow = reflow,
      cl = "fittexttree",
      height = height
    )
    gt$name <- grid::grobName(gt, "geom_lmannotate")
    gt

  }
)
