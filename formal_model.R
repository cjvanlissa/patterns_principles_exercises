# Set random seed
set.seed(62425044)
# Set simulation parameters
n <- 100
# Simulate exogenous nodes
external_event <- rnorm(n = n)
needs <- rnorm(n = n)
# Simulate endogenous nodes
intrinsic_motivation <- -(0.22 * needs + 0.29 * external_event) + rnorm(n = n)
wellbeing <- 0.15 * needs - 0.12 * intrinsic_motivation + rnorm(n = n)
df <- data.frame(
external_event = external_event,
intrinsic_motivation = intrinsic_motivation,
needs = needs,
wellbeing = wellbeing
)
