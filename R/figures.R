plot_all_parameters <- function(df, year_select = "", 
                                parameter_group = c("physical","biological",
                                                    "bacteriological",
                                                    "major_constituents",
                                                    "trace_constituents",
                                                    "operations", "all"),
                                station_list = c("Raw","PreFM","Channel",
                                                 "PreGAC","Clearwell", "all")){

  if(parameter_group %in% df$parm_tag){
    df <- df %>% filter(parm_tag %in% parameter_group)
  } else {
    df <- df
  }
  
  if(station_list %in% df$station){
    df <- df %>% filter(station %in% station_list)
  } else {
    df <- df
  }
  
  df$station <- factor(df$station, levels = c("Raw","PreFM","Channel",
                                              "CW_channel1","CW_channel2", 
                                              "MMFA","MMFL","PreGAC","Clearwell"))
  
  n_parms <- length(unique(df$parameter))
  parms_list <- unique(df$parameter)
  
  for(i in 1:n_parms){
 
    parm_fig(df = df, year_select = year_select, var = parms_list[i])
  }
  
}

parm_fig <- function(df = "", year_select = "", var = "", 
                     outdir = "output"){
  
  df <- df %>% 
  filter(parameter == var) %>%
  mutate(week = week(datetime_ymd.hms),
           year = year(datetime_ymd.hms))
  
  df$station <- factor(df$station, 
                       levels = c("Raw","PreFM","Channel",
                                  "CW_channel1","CW_channel2", 
                                  "MMFA","MMFL","PreGAC","Clearwell"))
  
  trend_plot <- trend_fig(df = df, year_select = year_select, var = var)
  trend_plot <- trend_plot + theme(plot.title.position = "plot")
  
  boxplot_plot <- ggplot(data = df, aes(x = station, 
                                        y = result, col = week)) +
    geom_boxplot(fill = NA) + 
    geom_point(position = position_dodge2(width = 0.5)) +
    scale_y_continuous(labels = scales::comma) +
    labs(subtitle = "Historical distribution", x = "", y = "result",
         caption = paste("Year Range:", min(df$year), "-", max(df$year))) +
    theme_bpwtp() +
    theme(legend.position = "bottom",
          plot.title.position = "plot", 
          axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.8)) +
    scale_color_viridis_c(begin = 0.2, end = 0.8, 
                          option = "D", name = "Week")
    
  
  full_plot <- plot_grid(trend_plot, boxplot_plot, ncol = 2, 
                         rel_widths = c(0.75,0.35), 
                         align = "hv", axis = "tb")
  outname = paste0(year_select,"_",var,".png")
  outpath = file.path(outdir, outname)
  ggsave(full_plot, filename = outpath, width = 7, height = 6, scale = 1, device = "png")
  return(full_plot)
}

trend_fig <- function(df = "", year_select = "", var = "") {
  p <- ggplot(data = df %>% 
                filter(year == year_select),
              aes(x = week, y = result, col = station)) +
    geom_point(data = df, aes(x = week, y = result), col = "grey", alpha = 0.4) +
    stat_summary(data = df, geom = "line", fun = "mean", col = "grey", size = 1) +
    geom_line() +
    geom_point(size = 2, col = "white") +
    geom_point(size = 1) +
    facet_grid(station~., scales = "free_y")+
    scale_y_continuous(labels = scales::comma) +
    labs(title = var, subtitle = paste(year_select, "trend with historical data"),
         x = "week", y = "result") +
    theme_bpwtp() +
    theme(legend.position = "bottom")  +
    scale_color_viridis_d(begin = 0.2, end = 0.8, option = "D")
  
  return(p)
}



tthm_fig <- function(df = "thms", year_select = ""){
  
  df <- df %>% 
    mutate(week = week(datetime_ymd.hms),
           year = year(datetime_ymd.hms))
  
  df$station <- factor(df$station, levels = c("Raw","PreFM","Channel",
                                              "CW_channel1","CW_channel2", 
                                              "MMFA","MMFL","PreGAC","Clearwell"))

  trend_plot <- trend_fig(df = df, year_select = year_select, 
                          var = unique(df$parm_unit))
  trend_plot <- trend_plot + 
        facet_grid(parm_unit ~ station, scales = "free_y") +
    geom_hline(aes(yintercept = hist.max), col = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = hist.min), col = "red", linetype = "dashed") +
    labs(title = "Individual trihalomethanes", 
         y = expression(Concentration~(mu*g/L))) 
  
  trend_plot$data$station <- factor(trend_plot$data$station, 
                                    levels = c("PreFM","Channel","PreGAC","Clearwell"))
  
  summary_plot <- ggplot(data = df %>% 
                          filter(year == year_select) %>%
                           distinct(), 
                        aes(x = week, y = TTHMs, 
                        col = station, shape = station)) +
    geom_line() +
    geom_point(size = 2, col = "white") +
    geom_point(size = 1) +
    labs(title = "total trihalomethanes (tTHMs)", 
         x = "week", 
         y = expression(Concentration~(mu*g~L^-1))) +
    theme_bpwtp() +
    scale_color_viridis_d(begin = 0.2, end = 0.8, option = "D")
  
  summary_plot$data$station <- factor(summary_plot$data$station, 
                                    levels = c("PreFM","Channel","PreGAC","Clearwell"))
  
  full_plot = plot_grid(trend_plot, summary_plot, ncol = 1, axis = "l", 
                        align = "hv", rel_heights = c(1,0.5), labels = "auto")
  
  return(full_plot)
  
}
