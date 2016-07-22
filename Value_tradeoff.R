# ============================================================================
# This script can be used to reproduce the figure 1 of the CEER paper
# Vezer et al., 2016
#
# (Alexander Bakker, 2016)
# ============================================================================

# libraries
require(ggplot2)
source("set_theme_AB.R")

# Define x-values
x1  = 0:100
x2  = 100:0 # mirrored x-values

# exponential time scale
tau = 40

Simplicity = exp(-x1/tau)
Completeness = exp(-x2/tau)

df1 = rbind(data.frame(x=x1, y=Simplicity,   Value="Simplicity"),
            data.frame(x=x1, y=Completeness, Value="Completeness"))

ltypes = c('Simplicity'   = 'dashed',
           'Completeness' = 'solid')

p1 <-
  ggplot(data=df1, aes(x=x, y=y, linetype=Value)) +
# Plot data
  geom_line(data=df1, color='black') +
# Define labels and legends (scales)  
  scale_x_continuous("Complexity",breaks=NULL) +
  scale_y_continuous("Epistemic\nvalue",breaks=NULL, limits=c(-0.1,1.3)) +
  scale_linetype_manual(NULL, values=c('dashed','solid')) +
# remove border
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(angle=0, margin = margin(r=10)),
        axis.title.x=element_text(margin = margin(t=10, b=10))) +
# apply arrows as axes
  geom_segment(aes(x=-1, xend=110, y=0, yend=0),    linetype='solid', color ="black",
               arrow=arrow(angle = 30, type = "closed", length = unit(0.06, "npc"))) +
  geom_segment(aes(x=-1, xend=-1,   y=0, yend=1.3), linetype='solid', color ="black",
               arrow=arrow(angle = 30, type = "closed", length = unit(0.06, "npc")))


pdf("Value_tradeoff.pdf", paper="special", width=10, height=4); print(p1); dev.off() 

