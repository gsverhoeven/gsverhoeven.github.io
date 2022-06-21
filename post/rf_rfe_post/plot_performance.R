plot_performance <- function(data, axis_label = "", plot_title = "", 
                             facet_var = "ds_name", x_var = "algo") {
  ggplot(data, 
         aes(x = reorder(get(x_var), performance), 
             y = performance*100, 
             col = factor(get(x_var)))) +
    geom_point() +
    geom_segment(aes(xend = get(x_var), 
                     y = performance_lower*100, 
                     yend = performance_upper*100)) +
    #expand_limits(y = 0) +
    geom_label(aes(label = paste0(round(performance*100,0), "%")), nudge_y = -0.02, nudge_x = +0.3) +
    coord_flip() +
    facet_wrap(~ get(facet_var)) +
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    labs(y = axis_label, x = "", title = plot_title) +
    theme(legend.position = 'none')
}
