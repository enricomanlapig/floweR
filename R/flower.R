#' Draw flowers
#'
#' @param my_df A data frame
#' @param my_grouping_var A field that distinguishes each flower
#' @param my_metric_var A numeric field of petal lengths
#' @param my_color_var A field to map petal color
#' @param my_x_margin a numeric field that controls how far the flowers are horizontally positioned from each other
#' @param my_y_jitter a numeric field that controls the random vertical placement of each flower
#' @param my_curvature a numeric value that indicates the width of each petal
#' @param my_angle a numeric value between 0 and 180 that indicates where the widest point of curvature falls on the petal. Values closer to zero put this point closer to the middle of the flower, while values further from zero put the point further away.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr enquo
#' @importFrom dplyr pull
#' @importFrom dplyr row_number
#' @importFrom dplyr count
#' @importFrom dplyr first
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom stats runif
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_curve
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 aes
#'
#' @export

draw_flowers <-
  function(my_df,
           my_grouping_var,
           my_metric_var,
           my_color_var = NULL,
           my_x_margin = 0,
           my_y_jitter = 0,
           my_curvature = 0.5,
           my_angle = 90) {

    original_order <- group_number <- max_width <- num_obs <- degree_increment <- x_pos <- y_pos <- theta <- x_end <- y_end <- max_length <- NULL

    my_color_var <- enquo(my_color_var)
    my_metric_var <- enquo(my_metric_var)
    my_grouping_var <- enquo(my_grouping_var)
    my_x_margin <- enquo(my_x_margin)
    my_y_jitter <- enquo(my_y_jitter)

    degree_to_radian_factor <- pi / 180

    my_df %>%
      mutate(original_order = row_number()) %>%
      arrange(!!my_grouping_var,!!my_metric_var) -> tmp_df

    tmp_df <- cbind(tmp_df,
                   group_number = rep(1:length(unique(
                     pull(tmp_df,!!my_grouping_var)
                   )),
                   times = count(tmp_df,!!my_grouping_var)$n))


    tmp_df %>%
      arrange(original_order) %>%
      mutate(
        row_num = row_number(),
        group_number = group_number,
        max_width = !!my_x_margin + 2 * max(!!my_metric_var)
      ) %>%

      group_by(!!my_grouping_var) %>%

      mutate(
        num_obs = n(),
        x_pos  = group_number * max_width,
        y_pos = runif(1, max = !!my_y_jitter * max_width),
        degree_increment = 360 / num_obs,
        theta = cumsum(degree_increment),
        x_end = x_pos +
          ifelse(
            theta < 180 * degree_to_radian_factor |
              theta > 270 * degree_to_radian_factor,
            cos(theta * degree_to_radian_factor),
            -cos((180 - theta) * degree_to_radian_factor)) * !!my_metric_var,
        y_end = y_pos + sin(theta * degree_to_radian_factor) * !!my_metric_var,
        label = ifelse(!!my_grouping_var == first(!!my_grouping_var), !!my_grouping_var, NA)
      ) %>%

      ungroup() %>%
      mutate(max_length = max(!!my_metric_var)) %>%


      ggplot(aes(
        x = x_pos,
        y = y_pos,
        xend = x_end,
        yend = y_end
      )) +

      geom_curve(
        aes(color = !!my_color_var),
        curvature = my_curvature,
        angle = my_angle,
        ncp = 10,
        lineend = "round"
      ) +

      geom_curve(
        aes(color = !!my_color_var),
        curvature = -my_curvature,
        angle = -my_angle,
        ncp = 10,
        lineend = "round"
      ) +

      geom_text(aes(x = x_pos,
                    y = y_pos - max_length,
                    label = !!my_grouping_var),
                na.rm = TRUE) +

      coord_fixed(ratio = 1) +
      theme_void() -> g

    return(g)

  }

