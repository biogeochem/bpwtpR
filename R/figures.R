#' Plot all parameters
#'
#' Plot all possible parameters
#'
#' @param df labdat dataset
#' @param year_select year of interest
#' @param parameter_group data grouping
#' @param station_list station
#' @param outdir string. Path to the directory in which to save plot pngs
#'
#' @return ggplot
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
plot_all_parameters <- function(df, year_select = "",
                                parameter_group = c("physical","biological",
                                                    "bacteriological",
                                                    "majorConstituents",
                                                    "traceConstituents",
                                                    "operations", "THM", "all"),
                                station_list = c("Raw", "Clearwell",
                                                 "PreGAC",
                                                 "PreFM", "FM",
                                                 "Channel",
                                                 "Channel 1","Channel 2",
                                                 "MMF1", "MMF12", "MMFA", "MMFL",
                                                 "Combined Stations",
                                                 "all"),
                                outdir){
  for (i in 1:length(parameter_group)) {
    if(parameter_group[i] %in% df$parm_tag){
      df <- df %>% filter(parm_tag %in% parameter_group)
    }
  }

  for (i in 1:length(station_list)) {
    if(station_list[i] %in% df$station){
      df <- df %>% filter(station %in% station_list)
    }
  }

  df$station <- factor(df$station)

  parms_list <- unique(df$parameter)

  for(i in 1:length(parms_list)){
    #print(parms_list[i])
    parm_fig(df = df, year_select = year_select, var = parms_list[i], outdir)
  }

}

#' Plot parameter
#'
#' Plot parameter figure with trend and boxplot distribution
#'
#' @param df dataset
#' @param year_select year of interest
#' @param var parameter variable
#' @param outdir location to save output
#'
#' @return ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot geom_boxplot geom_point scale_y_continuous labs
#'   theme scale_color_viridis_c ggsave element_text
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate
#' @importFrom cowplot plot_grid
#' @importFrom lubridate week year
parm_fig <- function(df = "", year_select = "", var = "",
                     outdir){

  df <- df %>%
    filter(parameter == var) %>%
    mutate(station = factor(station))

  trend_plot <- trend_fig(df = df, year_select = year_select, var = var)
  trend_plot <- trend_plot + theme(plot.title.position = "plot")

  boxplot_plot <- ggplot(data = df, aes(x = station,
                                        y = result, col = week_num)) +
    geom_boxplot(fill = NA, outlier.colour = "white") +
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

  outname = paste0(var,".png")
  outpath = file.path(outdir, outname)
  ggsave(full_plot, filename = outpath, width = 4, height = 3, scale = 1.5,
         device = "png")

  return(full_plot)
}

#' Plot yearly trend
#'
#' Plot parameter yearly trend
#'
#' @param df data frame
#' @param year_select year of interest
#' @param var variable of interest
#'
#' @return ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot stat_summary geom_line geom_point facet_grid
#'   scale_y_continuous labs theme scale_color_viridis_d
#' @importFrom dplyr filter
trend_fig <- function(df = "", year_select = "", var = "") {

  p <- ggplot(data = filter(df, year == year_select, !is.na(station)),
              aes(x = week_num, y = result, col = station)) +
    geom_point(data = df, aes(x = week_num, y = result), col = "grey", alpha = 0.4) +
    stat_summary(data = df, geom = "line", fun = "mean", col = "grey", linewidth = 1) +
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

#' Plot tTHM
#'
#' Plot total trihalomethanes
#'
#' @param df default is thms
#' @param year_select year of interest
#'
#' @return ggplot
#' @export
#'
#' @importFrom lubridate week year
#' @importFrom ggplot2 facet_grid geom_hline labs ggplot geom_line geom_point
#'  scale_color_viridis_d
#' @importFrom dplyr filter
#' @importFrom cowplot plot_grid
tthm_fig <- function(df = "thms", year_select = ""){

  df$station <- factor(df$station)

  trend_plot <- trend_fig(df = df, year_select = year_select,
                          var = unique(df$parm_unit))
  trend_plot <- trend_plot +
    facet_grid(parm_unit ~ station, scales = "free_y") +
    geom_hline(aes(yintercept = hist.max), col = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = hist.min), col = "red", linetype = "dashed") +
    labs(title = "Individual trihalomethanes",
         y = expression(Concentration~(mu*g/L)))

  trend_plot$data$station <- factor(trend_plot$data$station)

  summary_plot <- ggplot(data = df %>%
                          filter(year == year_select) %>%
                           distinct(),
                        aes(x = week_num, y = TTHMs,
                        col = station, shape = station)) +
    geom_line() +
    geom_point(size = 2, col = "white") +
    geom_point(size = 1) +
    labs(title = "total trihalomethanes (tTHMs)",
         x = "week",
         y = expression(Concentration~(mu*g~L^-1))) +
    theme_bpwtp() +
    scale_color_viridis_d(begin = 0.2, end = 0.8, option = "D")

  summary_plot$data$station <- factor(summary_plot$data$station)

  full_plot = plot_grid(trend_plot, summary_plot, ncol = 1, axis = "l",
                        align = "hv", rel_heights = c(1,0.5), labels = "auto")

  return(full_plot)

}
