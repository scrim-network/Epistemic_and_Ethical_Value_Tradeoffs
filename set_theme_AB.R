
theme_ali <- function (base_size = 16, base_family = "") 
{
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"), 
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1), 
    text = element_text(family = base_family, face = "plain",
                        colour = "black", size = base_size, 
                        hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                        margin = margin(), debug = FALSE), 
    
    axis.text = element_text(size = rel(0.8), colour = "black"), 
    strip.text = element_text(size = rel(0.8)), 
    axis.line = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(vjust = 1), 
    axis.text.y = element_text(hjust = 1), 
    axis.ticks = element_line(colour = "grey50"), 
    axis.title.x = element_text(margin = margin(t=5, b=5)),
    axis.title.y = element_text(angle=90, margin = margin(r=5, l=5)), 
    axis.ticks.length = unit(0.15, "cm"), 
   # axis.ticks.margin = unit(0.1, "cm"), 
    
    legend.background = element_rect(colour = NA), 
    legend.margin = unit(0.2, "cm"), 
    legend.key = element_blank(), 
    legend.key.size = unit(1.2, "lines"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    legend.text = element_text(size = rel(0.8)), 
    legend.text.align = NULL, 
    legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0), 
    legend.title.align = NULL, 
    legend.position = "right", 
    legend.direction = NULL, 
    legend.justification = "center", 
    legend.box = NULL, 
    
    panel.background = element_rect(fill = NA, colour = "black"), 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.margin = unit(0.25, "lines"), 
    panel.margin.x = NULL, 
    panel.margin.y = NULL, 
    panel.ontop = FALSE,
    
    strip.background = element_rect(fill = "grey80", colour = NA), 
    strip.text.x = element_text(), 
    strip.text.y = element_text(angle = -90),
    strip.switch.pad.grid = unit(0, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
   
    plot.background = element_rect(colour = "white"), 
    plot.title = element_text(size = rel(1.2)), 
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}


theme_set(theme_ali())
