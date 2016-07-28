# ============================================================================
# Reproduces figure 2 of Vezer et al (2016)
#
# Alexander Bakker (2016)
# ============================================================================

# ============================================================================
# load necessary libraries and source help scripts
library(reshape2)            # for reshaping data.frame for use in ggplot
library(ggplot2)             # enables plotting via ggplot
source("set_theme_AB.R")     # changes default settings
source("ggplot_dual_axis.R") # enables dual axis in ggplot (requires library 'grid')
library(grid)
library(cowplot)             # used to combined and allign two graphs

# ============================================================================
# Plot economic optimization of levee heightening
# ============================================================================

# ============================================================================
# First reshape data.frame
df.m   <- melt(data=df,
               id.vars=c("Return_period", "Flood_prob", "Heightening"), # first reshape
               variable.name="cost_type",
               value.name="costs")

# ============================================================================
# Create separate data.frame for optimal point to enable separate legend
df.opt <- data.frame(x=df$Heightening[opt],
                     y=df$Total_costs[opt],
                     shape='optimal')

# ============================================================================
# Define legend new scales

# shapes
shapes        <- c('optimal' = 21)

# colors
colors        <- c("magenta","deepskyblue","darkgreen", "white", "gray", "black")  # colors (white is used to create white space)
spaces        <- 47                                                                # used to allign with legend other plot
names(colors) <- c("Investment",
                   "Risk",
                   "Total costs",
                   paste(rep(" ",spaces), collapse = ""),                          # white and name length used to tweak nice allignment
                   "100-yr return level",
                   "Economic optimal level")            
df.m$cost_type <- factor(df.m$cost_type, c(levels(df.m$cost_type), "A","B","C"))   # three levels added to factor to include
                                                                                   # additional levels in legend

# second x-axis (return periods)
Rperiods <- c((1:9)*10,(1:9)*100,(1:9)*1000,(1:9)*10000,(1:10)*100000)
p        <- 1/Rperiods
Rbreaks  <- log((p/p0)^(-1/alpha)) - X50 # '- X50' because shown with respect to pre-Katrina safety levels
Rlabels  <- rep("", length(Rbreaks))     # first assing all ticks the label ""
lbreaks  <- c(100,1000,10000,100000)     # then pick ticks to assing real label
rows     <- match(lbreaks, Rperiods)     # 
Rlabels[rows] <- c("100","1,000","10,000","100,000") # and assing associated label to the ticks

# ============================================================================
# Plot main plot
p0 <- 
  ggplot(data=df.m, aes(y=costs, color=cost_type)) +
# plot data
  geom_vline(xintercept=H100, color="gray",  size=1.5, linetype=1) +
  geom_vline(xintercept=Hopt, color="black", size=1.5, linetype="dashed") +
  geom_line(aes(x=Heightening), size=1.5) +
  geom_point(data=df.opt, aes(x=x, y=y, shape=shape), fill='gold', color='black', size=4) +
# Define labels
  labs(x        = "Levee heightening compared to pre-Katrina levels [meters]",
       y        = "Costs [billion U.S. $]",
       color    = NULL,
       linetype = NULL,
       shape    = NULL) +
# Define legends (scales)
  scale_color_manual(labels=c("Investment", "Risk", "Total costs", paste(rep(" ",spaces), collapse = ""), "100-yr return level", "Economic optimal level"),
                     values=c("magenta","deepskyblue","darkgreen", "white", "gray", "black"), # white is only defined to create white space 
                     drop=F) +
  scale_shape_manual(values=shapes, labels='Optimal point') +
# Set order of the legends  
  guides(color = guide_legend(order=1),
         shape = guide_legend(order = 2)) +
# Some final tweaking
theme(plot.margin=unit(c(0.0,1,1,1), "cm") )

# ============================================================================
#Create double x-axis

# Add levee heightening on first x-axis
p1 <- p0 +
  scale_x_continuous(breaks=(0:5)/foot, labels=0:5) # translate levee heightenings from 'feet' to 'meters'
  

# Add return periods on second x-axis and remove axis title
p2 <- p0 +
  scale_x_continuous(NULL, breaks=Rbreaks, labels=Rlabels)

g1 <- ggplot_dual_axis(p1, p2, which.axis = "x")
plot(g1)

# ============================================================================
# ============================================================================


# ============================================================================
# Plot risk to life
# ============================================================================

plot.risk2life <- function(df.r) {
  # Subselection with relevant values
  df.r$ev.min[which(df.r$ev.min<lbev)] <- lbev # everything outside is provided min value of plotted area
  df.r$ev.max[which(df.r$ev.max<lbev)] <- lbev
  
  katrina.evac <- data.frame(rates=c(0.8,0.9), storm=rep("Katrina",2))  # evacuation rate Katrina
  
  # ============================================================================
  # Define legend new scales
  
  # Colors of risk perception
  risk.colors <- c("Tolerable"           = "green",
                   "Marginally accepted" = "gold",
                   "Less accepted"       = "orange",
                   "Intolerable"         = "red")
  
  # Katrina colors
  katrina.colors <- c("appel" = "blue")
  
  p <- 
    ggplot(df.r, aes(x=Heightening, ymin=ev.min, ymax=ev.max, fill=Risk.level)) +
    # Plot data
    geom_ribbon() +
    geom_line(data=df.r, aes(y=ev.max), color="black") +
    geom_line(data=df.r, aes(y=ev.min), color="black") +
    geom_vline(xintercept=H100, color="gray", size=2, linetype=1) +
    geom_vline(xintercept=Hopt, color="black", size=1.5, linetype="dashed") +
    geom_hline(data=katrina.evac, aes(yintercept=rates, colour="appel"), size=1.5, linetype="dashed") +
    # Define labels
    labs(x="\nReturn period [years]\n",
         y="Evacuation rate [%]",
         fill="Individual risk to life",
         color=NULL) +
    # Define legend scales
    scale_x_continuous(breaks=Rbreaks,labels=Rlabels) +
    scale_y_continuous(breaks=(5:10)/10, labels=(5:10)*10) +
    scale_fill_manual(values  = risk.colors, breaks = names(risk.colors), labels  = names(risk.colors)) + 
    scale_color_manual(values  = "blue", labels  = "Upper and lower estimates of\nevacuation rate for Katrina") + 
    guides(fill = guide_legend(order = 1), 
           colour = guide_legend(order = 2))
  
  return(p)
  
}

p5 <- plot.risk2life(df.r.av)
p6 <- plot.risk2life(df.r.mx)

g5 <- ggplotGrob(p5)
g6 <- ggplotGrob(p6)



pdf("fig2.pdf", paper="special", width=10, height=10/1.6)
print(plot_grid(g6, g1, ncol=1, align = "v", rel_heights = c(2, 2)))
dev.off()

  
