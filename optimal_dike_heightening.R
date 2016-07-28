# ============================================================================
# Calculates investment costs, risk and total costs for different dike
# heightenings in New Orleans, levee ring North 1 (based on Jonkman et al, 2009)
#
# Alexander Bakker (2016)
# ============================================================================

# ============================================================================
# Parameters for economic optimization
V0       <- 15            # Damage in case of failure [billon dollar]
delta    <- 0.02          # Discount factor (4% discount rate -
                          #                  1% economic growth - 
                          #                  1% increase flood probability due to slr)
p0       <- 0.01          # failure probability (due to overtopping)
                          # initial level H0 = 2.7432 meter (9 feet)
alpha    <- -log(0.1)/4   # exponential flood frequency coefficient (for X in ft)
X50      <- -1.2          # return level belonging to 50-year return period [ft]

# Estimated investment costs for different dike heightenings
I_table <- data.frame(
  X  = c( X50,   0,   2,   4,   6,   8,   10,  12,   14,  16),   # heightenings compared to current level [ft]
  N1 = c( 2.0, 2.2, 2.4, 2.6, 2.9, 3.1, 3.35, 3.6, 3.85, 4.1))   # update costs with respect to post-Katrina state [billion dollars]

foot     <- 0.3048                                               # length of 1 ft [m]

# ================================================================================
# Calculate failure probabilities, risks and costs for different levee heightenings
X             <- (-120:1600)/100                         # Considered levee heightenings [ft]
Flood_prob    <- p0 * exp(- alpha * (X))                 # Annual flood probability
Risk          <- V0/delta * Flood_prob                   # Approximation used by Jonkman et al (2009)
Investments   <- approx(x=I_table$X, y=I_table$N1, X)$y  # Linearly interpolates from investment table
Return_period <- round(1/Flood_prob,0)
Heightening   <- X - X50                                 # levee heightening with respect to Post-Katrina levels [ft]
Total_costs   <- Risk + Investments

# Store in data.frame (df)
df            <- data.frame(Flood_prob, Risk, Investments, Return_period, Heightening, Total_costs)

# Calculate 
H100 <- Heightening[which(Return_period == 100)]         # Current standard (Return period is 100 years)
opt  <- which(Total_costs == min(Total_costs))           # Optimal point in table (minimal costs)
Hopt <- Heightening[opt]                                 # Optimal levee heightening
Ropt <- Return_period[opt]                               # Optimal design return period

