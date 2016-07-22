# ============================================================================
# This script reproduces and combines the simple studies in order to
# showcase how a simple model framework could be utilized to explore
# financial and risk-to-life tradeoffs in the New Orleans Metro-bowl.
#
# Financial efficiency is assessed adopting:
# - 'A Dutch perspective on coastal Louisiana flood risk reduction 
#    and landscape stabilization' (Dijkman 2007, Jonkman et al. 2009).
#
# And risk-to-life is emulated from:
# - 'Risk to life due to flooding in post-Katrina New Orleans'
#   (Miller et al. 2015)
#
# Alexander Bakker (2016)
# ============================================================================

# ============================================================================
# clear environment
rm(list = ls())
graphics.off()

# ============================================================================
# Run scripts

# Estimation optimal dike heightening for levee ring North 1
# (Jonkman et al, 2009)
source("optimal_dike_heightening.R") 

# Estimation of risk-to-life as a function from dike heightening and evacuation rate
# (based on Jonkman et al (2009) and Miller et al (2015))
source("risk_to_life.R")

# Construct tradeoff figure
source("construct_tradeoff_figure")

