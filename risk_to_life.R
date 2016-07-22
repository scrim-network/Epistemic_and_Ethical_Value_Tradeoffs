# ============================================================================
# Calculates risk to life for different dike heightenings and evacuation rates
# heightenings in New Orleans, Central Bowl
# (based on Jonkman et al 2009 and Miller et al 2015)
#
# Alexander Bakker (2016)
# ============================================================================

# ============================================================================
# Parameters for risk-to-life calculations

# High-end estimate for the probability of being killed by flood in case of flood
# (like in Miller 2015 max. value applied for entire bowl)
Fd          <- 0.053 # max value applied for entire bowl (i.e. high end estimate)

acc.risk    <- 1e-4  # marginally acceptable risk
tol.risk    <- 1e-6  # tolerable risk
less.acc    <- 1e-5  # less accept 

# =======================================================================================
# Calculate necessary evacuation rates for different dike heightenings and risk averseness

ev.rate.opt.tol  <- 1 - tol.risk * Ropt / Fd # ev.rate required for tolerable risk in case of financially optimal dike heights
ev.rate.100.less <- 1 - less.acc * 100  / Fd # ev.rate required for less acc  risk in case of financially current dike heights
ev.rate.100.not  <- 1 - acc.risk * 100  / Fd # ev.rate required for marg acc  risk in case of financially current dike heights

evacuation.rates <- seq(0.5,1,by=0.01)       # vector of considered evacuation rates
X                <- (-12:160)/10             # vector of considered dike heightenings with respect to current level

# Create data.frame
df.r <- data.frame(
  Risk.level  = rep(c(  "Tolerable", "Marginally accepted", "Less accepted", "Intolerable"), each = length(X)),
  Risk.min    = rep(c(         1e-6,                  1e-5,            1e-4,             1), each = length(X)),
  Risk.max    = rep(c(            0,                  1e-6,            1e-5,          1e-4), each = length(X)),
  Heightening = rep(X - X50, 4) # X50 defined in <optimal_dike_heightening.R>
)

df.r$Return <- 1 / (p0 * exp(- alpha * (X)) )
df.r$ev.min <- 1 - ( df.r$Risk.min * df.r$Return / Fd )
df.r$ev.max <- 1 - ( df.r$Risk.max * df.r$Return / Fd )

# make subset of real values
df.r <- df.r[which(df.r$ev.max>=0),]
