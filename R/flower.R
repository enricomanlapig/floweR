#' Draw flowers
#'
#'
#' @export
#' @param my_df data frame
#' @param my_grouping_var field within data frame
#' @param my_metric_var numeric field within data frame
#' @param my_color_var field within data frame
#' @param my_x_margin numeric variable
#' @param my_y_jitter numeric variable
#' @param my_curvature numeric variable
#' @param my_angle numeric variable between 0 and 180


draw_flowers <-
  function(my_df,
           my_grouping_var,
           my_metric_var,
           my_color_var = NULL,
           my_x_margin = 0,
           my_y_jitter = 0,
           my_curvature = 0.5,
           my_angle = 90) {

    my_color_var <- enquo(my_color_var)
    my_metric_var <- enquo(my_metric_var)
    my_grouping_var <- enquo(my_grouping_var)
    my_x_margin <- enquo(my_x_margin)
    my_y_jitter <- enquo(my_y_jitter)

    degree_to_radian_factor <- pi / 180

    my_df %>%
      mutate(original_order = row_number()) %>%
      arrange(!!my_grouping_var,!!my_metric_var) -> my_df

    my_df <- cbind(my_df,
                   group_number = rep(1:length(unique(
                     pull(my_df,!!my_grouping_var)
                   )),
                   times = count(my_df,!!my_grouping_var)$n))


    my_df %>%
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

