make_heatmap <- function(df, division_name){
 
  df_agg <- df %>% 
    group_by(race_name, race_type) %>% 
    summarise(Blocks = mean(home_block), 
              Completions = mean(home_comp),
              Fouls = mean(home_foul),
              Distance_passed = mean(home_pass), 
              Running_w_ball = mean(home_rush), 
              Cas_inflicted = mean(home_cas),
              Cas_suffered = mean(away_cas), 
              Touchdowns = mean(team_score),
              size = n())
  
  varnames <- c("Blocks", "Fouls", "Cas_inflicted",  "Distance_passed", "Completions", "Running_w_ball", "Cas_suffered")
  yorder <- data.frame(yorder = 1:length(varnames), variable = varnames)
  
  df_long <- df_agg %>%
    pivot_longer(cols = !c(race_name, race_type, size), names_to = "variable") %>%
    group_by(variable) %>%
    mutate(sd_value = scale(value)) %>%
    left_join(yorder, by = "variable")
  
  df_long <- hclust_order(df_long, 
                          xvar = "race_name", 
                          yvar = "variable", 
                          value_var = "value",
                          clust_method = "complete",
                          dist_method = "euclidean")
  
  p1 <- ggplot(df_long, aes(x = reorder(race_name, cluster_order), y = 1, fill = race_type)) +
    geom_tile() + coord_flip() + labs(x = "", y = NULL) + scale_y_discrete(labels = NULL, breaks = NULL) 
  
  p2 <- ggorder_heatmap(df_long, 
                        xvar = "race_name", 
                        yvar = "variable", 
                        col_var = "sd_value", 
                        order_var = "cluster_order",
                        yorder_var ="yorder",
                        legend = FALSE,
                        label_var = "value", round.digits = 1) + coord_flip() +
    scale_y_discrete(position = "right")

  p <- p1 + p2 + plot_layout(widths = c(1, 6))
  
  
  
  gp <- p + plot_annotation(title = paste0('FUMBBL ' , division_name, ' division: team typology'))
  
  gp
}