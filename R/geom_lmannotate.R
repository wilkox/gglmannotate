#' Add an annotation describing a linear model to a ggplot2 plot
#'
#' @details
#'
#' `geom_lmannotate()` uses the 'ggfittext' package to fit text to genes. All
#' text drawing options available in `ggfittext::geom_fit_text()` (growing,
#' reflowing, etc.) are also available here. For full details on how these
#' options work, see the documentation for `ggfittext::geom_fit_text()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @param glue_exp An expression to be parsed by `glue::glue()`, to form the
#' annotation describing each linear model. The fitted model object returned by
#' `lm()` is available as the variable `model`
#' @param padding.x,padding.y `grid::unit` object, giving horizontal and vertical
#' padding around the text. Defaults to 1 mm and 0.1 lines respectively.
#' @param place Annotations will stick to this corner of their drawing area.
#' Default is "topright"; other options are "right", "bottomright", "bottom",
#' "bottomleft", "left", "topleft", "top", and "centre|center|middle"
#' @param region Annotations will be placed in this region. A named numeric
#' vector with elements 'xmin', 'xmax', 'ymin' and 'ymax' defining the region.
#' x and y plot dimensions are scaled between 0 and 1. For example, to place in
#' the top left quadrant of the plot: `region = c(xmin = 0, xmax = 0.5, ymin =
#' 0.5, ymax = 1)`
#' @param min.size Minimum font size, in points. If provided, annotations that
#' would need to be shrunk below this size to fit inside their drawing area
#' will not be drawn. Defaults to 0 pt.
#' @param grow If `TRUE` (the default), annotations will be grown as well as
#' shrunk to fill their drawing areas 
#' @param reflow If `TRUE (`FALSE` by default), annotations will be reflowed
#' (wrapped) to better fit their drawing areas
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2
#'
#' @section Aesthetics:
#'
#' \itemize{
#'   \item x,y (required to fit the linear model)
#'   \item colour
#'   \item size
#'   \item alpha
#'   \item family
#'   \item fontface
#'   \item angle
#' }
#'
#' @export
geom_lmannotate <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  glue_exp = NULL,
  padding.x = grid::unit(1, "mm"),
  padding.y = grid::unit(0.1, "lines"),
  place = "topright",
  region = NULL,
  min.size = 0,
  grow = TRUE,
  reflow = FALSE,
  ...
) {
  ggplot2::layer(
    geom = GeomLmAnnotate,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      glue_exp = glue_exp,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      region = region,
      grow = grow,
      reflow = reflow,
      ...
    )
  )
}

#' GeomLmAnnotate
#' @noRd
#' @import grid
#' @import ggfittext
#' @importFrom glue glue
#' @importFrom stringr str_replace
GeomLmAnnotate <- ggplot2::ggproto("GeomLmAnnotate", ggplot2::Geom,

  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    colour = NA, fill = "grey20", size = 0.5,
    linetype = 1, alpha = 1
  ),

  draw_key = ggplot2::draw_key_blank,

  setup_data = function(data, params) {

    data$is_faceted <- rep(length(unique(data$PANEL)) > 1, nrow(data))

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

  draw_group = function(
    data,
    panel_params,
    coord,
    glue_exp = NULL,
    padding.x = grid::unit(1, "mm"),
    padding.y = grid::unit(0.5, "mm"),
    place = "topright",
    region = NULL,
    min.size = 0,
    grow = TRUE,
    reflow = FALSE
  ) {

    # Check the 'region' argument
    if (! is.null(region)) {

      if (! is.numeric(region)) {
        stop("`region` must be a numeric vector")
      }

      if (! length(region) == 4) {
        stop("`region` must have named elements 'xmin', 'xmax', 'ymin' and 'ymax'")
      }

      if (! all(sort(names(region)) == sort(c("xmin", "xmax", "ymin", "ymax")))) {
        stop("`region` must have named elements 'xmin', 'xmax', 'ymin' and 'ymax'")
      }

      if (any(region > 1 | region < 0)) {
        stop("values in `region` must be between 0 and 1")
      }
    }

    is_faceted <- data$is_faceted[1]

    # Must have at least two points to draw a line
    if (nrow(data) < 2) return(grid::nullGrob())

    coords <- coord$transform(data, panel_params)

    # Fit the lm
    model <- lm(y ~ x, data)

    # Build a description of the model
    xxx <- stringr::str_replace("x", "x", "x")
    if (is.null(glue_exp)) {
      glue_exp <- "Adj.\u00A0R\u00B2\u00A0=\u00A0\\
                      {signif(summary(model)$adj.r.squared, 2)}, \\
                      intercept\u00A0=\u00A0\\
                      {signif(model$coefficients[1], 2)}, \\
                      slope\u00A0=\u00A0\\
                      {stringr::str_replace(signif(\\
                        model$coefficients[2], 2), '-', '\u2212')}, \\
                      p\u00A0=\u00A0\\
                      {f <- summary(model)$fstatistic;\\
                        p <- pf(f[1], f[2], f[3], lower.tail = F);\\
                        signif(p, 2)}"
    }
    label <- glue::glue(glue_exp)

    # Set up the data frame to pass on to makecontent.fittexttree
    t_data <- data[1, ]

    # Set the graphical parameters for the text
    t_data$label <- label
    t_data$colour <- ifelse(is.na(data$colour[1]), "black", data$colour[1])
    t_data$angle <- 0
    t_data$size <- 10

    # These parameters define the text drawing area for all annotations to be
    # drawn on a panel
    if (is.null(region)) {
      p_xmin <- ifelse(is_faceted, 0, 0.25)
      p_xmax <- 1
      p_ymin <- 0.5
      p_ymax <- 1
    } else {
      p_xmin <- region["xmin"]
      p_xmax <- region["xmax"]
      p_ymin <- region["ymin"]
      p_ymax <- region["ymax"]
    }

    # Set the x limits for this annotation's drawing area
    t_data$xmin <- p_xmin
    t_data$xmax <- p_xmax

    # Two algorithms for placing annotations within the panel text drawing
    # area, depending on how many groups are to be placed
    #
    # s is the estimated number of annotations that will fit into the panel
    # text drawing area at a fixed height. It is set based on whether or not
    # the plot is faceted, as faceted plots will typically have smaller panels
    s <- ifelse(is_faceted, 0, 6)
    # If there are s or fewer groups in this panel, it is better on balance
    # to use a fixed y-height, otherwise the annotations can end up weirdly
    # spread out
    if (data$group_n[1] <= s) {
      slice_y_height <- (p_ymax - p_ymin) / s
      t_data$ymax <- p_ymax - ((data$group_i[1] - 1) * slice_y_height)
      t_data$ymin <- p_ymax - (data$group_i[1] * slice_y_height)

    # If there are s + 1 or more groups for this panel, divide the y dimension
    # of the panel's text drawing area into n horizontal slices where n is
    # the number of annotations to be drawn in that panel. This annotation
    # will be assigned the ith slice
    } else {
      slice_y_height <- (p_ymax - p_ymin) / data$group_n[1]
      t_data$ymax <- p_ymax - ((data$group_i[1] - 1) * slice_y_height)
      t_data$ymin <- p_ymax - (data$group_i[1] * slice_y_height)
    }

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
      height = NULL
    )
    gt$name <- grid::grobName(gt, "geom_lmannotate")
    gt

  }
)
