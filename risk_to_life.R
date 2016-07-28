# ===========================================================================================
# Calculates risk to life for different dike heightenings and evacuation rates heightenings
# in New Orleans, Central Bowl (based on Jonkman et al 2009a, 2009b)
#
# Alexander Bakker (2016)
# ===========================================================================================

# ===========================================================================================
# Parameters for risk-to-life calculations

# Mortality rate among those exposed to flood
Fd.mx          <- 0.053 # max value, i.e. at deep locations or in vicinity of levee breaches
Fd.av          <- 0.01  # average value 

acc.risk    <- 1e-4  # marginally acceptable risk
tol.risk    <- 1e-6  # tolerable risk
less.acc    <- 1e-5  # less accept 

# ===========================================================================================
# Calculate necessary evacuation rates for different dike heightenings and risk averseness

# evacuation needs for average mortality rate
ev.rate.opt.tol.av  <- 1 - tol.risk * Ropt / Fd.av # for tolerable risk in case of financially optimal dike heights
ev.rate.100.less.av <- 1 - less.acc * 100  / Fd.av # for less acc  risk in case of current             dike heights
ev.rate.100.not.av  <- 1 - acc.risk * 100  / Fd.av # for marg acc  risk in case of current             dike heights

# evacuation needs for maximum mortality rate
ev.rate.opt.tol.mx  <- 1 - tol.risk * Ropt / Fd.mx # for tolerable risk in case of financially optimal dike heights
ev.rate.100.less.mx <- 1 - less.acc * 100  / Fd.mx # for less acc  risk in case of current             dike heights
ev.rate.100.not.mx  <- 1 - acc.risk * 100  / Fd.mx # for marg acc  risk in case of current             dike heights

lbev = 0.5                                   # lower bound of plotted evacuation rates

evacuation.rates <- seq(lbev,1,by=0.01)      # vector of considered evacuation rates
X                <- (-12:160)/10             # vector of considered dike heightenings relative to current level

# Create data.frame considering AVERAGE risk to life (Fd.av) within exposed area
df.r.av <- data.frame(
  Risk.level  = rep(c(  "Tolerable", "Marginally accepted", "Less accepted", "Intolerable"), each = length(X)),
  Risk.min    = rep(c(         1e-6,                  1e-5,            1e-4,             1), each = length(X)),
  Risk.max    = rep(c(            0,                  1e-6,            1e-5,          1e-4), each = length(X)),
  Heightening = rep(X - X50, 4) # X50 defined in <optimal_dike_heightening.R>
)
df.r.av$Return <- 1 / (p0 * exp(- alpha * (X)) )                     # Return periods
df.r.av$ev.min <- 1 - ( df.r.av$Risk.min * df.r.av$Return / Fd.av )  # boundaries for different risk classes
df.r.av$ev.max <- 1 - ( df.r.av$Risk.max * df.r.av$Return / Fd.av )

# Create data.frame considering MAXIMUM risk to life (Fd.mx) within exposed area
df.r.mx <- df.r.av 
df.r.mx$ev.min <- 1 - ( df.r.mx$Risk.min * df.r.mx$Return / Fd.mx )  # boundaries for different risk classes
df.r.mx$ev.max <- 1 - ( df.r.mx$Risk.max * df.r.mx$Return / Fd.mx )

# make subset of real values
df.r.av <- df.r.av[which(df.r.av$ev.max>=0),]
df.r.mx <- df.r.mx[which(df.r.mx$ev.max>=0),]
