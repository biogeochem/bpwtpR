#' BPWTP theme
#'
#' @param base_size font size
#' @param base_family font family
#'
#' @return theme
#'
#' @import ggplot2
theme_bpwtp <-  function(base_size = 8,
                          base_family = "") {
  ggplot2::"%+replace%"(
    ggplot2::theme_bw(base_size = base_size, base_family = base_family) ,

    ggplot2::theme(
      axis.ticks.length = grid::unit(0, "cm"),
      axis.line = ggplot2::element_line(colour = "black", size = rel(1)),
      panel.grid.major = ggplot2::element_line(color = "grey95"),
      panel.grid.minor = ggplot2::element_line(color = "grey95"),
      panel.border = ggplot2::element_rect(fill = NA, color = "black"),
      plot.caption = ggplot2::element_text(
        size = rel(0.8),
        hjust = 1,
        face = "italic"
      ),
      strip.background = ggplot2::element_rect(fill = "gray95", color = "black")
    )
  )
}
