#' Add an annotation describing a linear model to a ggplot2 plot
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2
#' @param facet_mode If TRUE (FALSE by default), will draw every annotation in
#' the top right corner regardless of how many groups there are
#'
#' @export
geom_lmannotate <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE, facet_mode = FALSE, ...) {
  ggplot2::layer(
    geom = GeomLmAnnotate, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, facet_mode = facet_mode, ...)
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

  draw_group = function(data, panel_params, coord, facet_mode) {

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

    # Build the basic parameters for the text
    t_data <- data[1, ]
    t_data$colour <- ifelse(is.na(data$colour[1]), "black", data$colour[1])
    t_data$angle <- 0
    t_data$size <- 10
    t_data$label <- label
    padding.x <- grid::unit(1, "mm")
    padding.y <- grid::unit(1, "mm")
    place <- "topright"
    min.size <- 0
    grow <- FALSE
    reflow <- TRUE
    height <- NULL

    t_data$xmin <- 0.25
    t_data$xmax <- 1

    # If there is only one group, or if facet_mode is set, give it the
    # top-right quadrant to play with
    if (data$group[1] == -1 | facet_mode) {
      t_data$ymin <- 0.5
      t_data$ymax <- 1

    # If there is more than one group, partition the y-dimension
    } else {
      n_groups <- 7
      t_data$ymax <- 1 - ((data$group[1] - 1) * 0.5 / n_groups)
      t_data$ymin <- t_data$ymax - (0.5 / n_groups)
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
      height = height
    )
    gt$name <- grid::grobName(gt, "geom_lmannotate")
    gt

  }
)
