# ======================================================================
# Thème graphique du projet FU-SAU
# ======================================================================

theme_fu <- function() {

  theme_minimal(
    base_size = 14
  ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        size = 12
      ),
      axis.title = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

}
