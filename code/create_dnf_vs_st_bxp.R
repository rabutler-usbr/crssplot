library(CoRiverNF)
library(ggplot2)

# This script creates a boxplot comparing the full hydrology to the stress test
# TODO: This should be moved to NFStats

# UI ----------
ofolder <- "C:/alan/CRSS/CRSS.Offc_Dev/results/"
# Full hydrology then stress test hydrology for the colors
plotColors <- c("#00BFC4", "#F8766D")

# create the boxplot -------------

FullQ = as.data.frame(cyAnnTot$LeesFerry['1906/'])
STQ = as.data.frame(cyAnnTot$LeesFerry['1988/'])
FullQ$scen = "Full Hydrology"
STQ$scen = "Stress Test Hydrology"
Q = rbind(FullQ, STQ)
Q$LeesFerry = Q$LeesFerry/1000000

names(plotColors) <- c("Full Hydrology", "Stress Test Hydrology")

gg = ggplot(Q, aes(x = scen, y = LeesFerry, fill = scen)) + 
  theme_light() + 
  stat_boxplot_custom(
    lwd = .25, 
    fatten = .7,
    outlier.size = .75
  )

gg <- gg + scale_fill_manual(values =  plotColors) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(
    x = "", y = "Annual Flow (million acre-feet)", 
    title = "Distrubution of Alternative Future Hydrology Scenarios",
    subtitle = "Colorado River Natural Flow at Lees Ferry Gaging Station, Arizona"
  ) + 
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 7.4, color = "grey21"),
    plot.title = element_text(size=8.9),
    axis.text.x = element_text(color = "black", size = 7.2),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.y = element_text(size=7.8)
  ) +
  annotate("text", label = "1906-2017",  x= 1, y=16, size = 2.65) + 
  annotate("text", label = "1988-2017", x=2 , y=14.75, size = 2.65)

ggsave(
  file.path(ofolder, 'FlowDistBoxplot.png'), 
  width = 3.5, 
  height = 3.5, 
  units = "in", 
  dpi = 600
)