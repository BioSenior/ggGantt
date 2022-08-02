#' Stat Gantt
#'
#' Computes the relevant summary statistics from the data:
#' minimum, first quartile, median, third quartile, and the maximum.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @export
#'
#' @examples
#' p <- ggplot(ToothGrowth, aes(supp, len))
#'
#' # gantt plot
#' p + stat_gantt(aes(fill = supp))
stat_gantt <- function(mapping = NULL, data = NULL, geom = "gantt",
                       position = "identity", show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatGantt,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
  )
}


StatGantt <- ggproto("StatGantt", Stat,
                     compute_group = function(data, scales) {
                       probs <- c(0, 0.25, 0.5, 0.75, 1)
                       qq <- quantile(data$y, probs, na.rm = TRUE)
                       out <- qq %>% as.list %>% data.frame
                       names(out) <- c("ymin", "lower", "middle",
                                       "upper", "ymax")
                       out$x <- data$x[1]
                       out
                     },
                     required_aes = c("x", "y")
)

#' A gantt plot
#'
#' The gantt plot uses bar charts to show the intrinsic relationship of
#' project, schedule, and other time-dependent system progress over time.
#'
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @export
#'
#' @examples
#' p <- ggplot(ToothGrowth, aes(supp, len))
#'
#' # gantt plot
#' p + geom_gantt(aes(fill = supp))
#'
#' # Add stroke and median points
#' p + geom_gantt(aes(fill = as.factor(dose)),
#'                color = "black", stroke = 0.1, width = 0.1,
#'                position = position_dodge(0.2))
geom_gantt <- function(mapping = NULL, data = NULL,
                       stat = "gantt", position = "identity",
                       ...,
                       #linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGantt,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      #linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

draw_panel_function <- function(data, panel_scales, coord){

  coords <- coord$transform(data, panel_scales) %>%
    mutate(lower = rescale(lower, from = panel_scales$y.range),
           upper = rescale(upper, from = panel_scales$y.range),
           middle = rescale(middle, from = panel_scales$y.range))

  # print(coords)

  med <- pointsGrob(x = coords$x,
                    y = coords$middle,
                    pch = coords$shape,

                    gp = grid::gpar(
                      col = coords$colour,
                      fill = scales::alpha(coords$fill, coords$alpha),
                      lwd = coords$stroke,
                      fontsize = coords$point_size * .pt
                    ))

  gTree(children = gList(GeomRect$draw_panel(data, panel_scales, coord),
                         med))
}


draw_key_gantt <- function (data, params, size)
{
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }

  lwd <- min(data$size, min(size)/4)
  rect_grob = rectGrob(width = unit(1, "npc") - unit(lwd, "mm"),
                       height = unit(1, "npc") - unit(lwd, "mm"),
                       gp = gpar(col = data$colour %||% NA, fill = alpha(data$fill %||% "grey20", data$alpha),
                                 lty = data$linetype %||% 1, lwd = lwd * .pt, linejoin = params$linejoin %||%
                                   "mitre", lineend = if (identical(params$linejoin, "round"))
                                     "round"
                                 else "square"))

  point_grob = pointsGrob(0.5, 0.5,
                          pch = data$shape,
                          gp = gpar(
                            col = alpha(data$colour %||% "black", data$alpha),
                            fill = alpha(data$fill %||% "black", data$alpha),
                            fontsize = (data$point_size %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke / 2,
                            lwd = (data$stroke %||% 0.5) * .stroke / 2
                          )
  )
  gTree(children = gList(rect_grob,
                         point_grob))
}

GeomGantt <- ggproto("GeomGantt", Geom,
                     extra_params = c("na.rm"),

                     setup_data = function(data, params) {
                       data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)

                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL
                       )
                       #print(data)
                     },
                     draw_panel = draw_panel_function,
                     default_aes = aes(fill = "grey20", colour = NA, shape = 21,
                                       point_size = 2, size = 0.1, linetype = 1,
                                       alpha = NA, width = NA, height = NA, stroke = NA),

                     required_aes = c("x", 'lower', "upper", "ymin", "ymax", "middle"),

                     draw_key = draw_key_gantt
)
