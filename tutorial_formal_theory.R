library(theorytools)
library(dagitty)

sdt <- dagitty("
dag {
external_event
healthy_development
integration
intrinsic_motivation
locus_of_causality
needs
wellbeing
external_event -> intrinsic_motivation
external_event -> locus_of_causality
external_event -> needs
integration -> healthy_development
integration -> wellbeing
intrinsic_motivation -> healthy_development
intrinsic_motivation -> wellbeing
locus_of_causality -> intrinsic_motivation
needs -> integration
needs -> intrinsic_motivation
needs -> wellbeing
}")

sdt

# Plot full DAG
plot(sdt)

# Prune DAG to focal relationship
sdt_pruned <- dagitty("
dag {
intrinsic_motivation -> wellbeing
external_event -> intrinsic_motivation
needs -> intrinsic_motivation
needs -> wellbeing
}")

sdt_pruned

# Generate and source simulation code
sim_code <- simulate_data(sdt_pruned, n = 100, run = FALSE)
sim_code
writeLines(sim_code, "formal_model.R")
source("formal_model.R")

# Descriptive statistics
desc <- worcs::descriptives(df)
print(desc, digits = 2)

# Density plot
plot(density(df$intrinsic_motivation))

# Bivariate regression
summary(lm(intrinsic_motivation~external_event, df))

# Scatterplot
plot(df$needs, df$intrinsic_motivation)

# Scatterplot with regression line
plot(df$needs, df$intrinsic_motivation)
abline(lm(intrinsic_motivation ~ needs, df))

# Interaction regression
res <- lm(intrinsic_motivation~external_event*needs, df)
sum_res <- summary(res)
sum_res
# Is the interaction effect significant?
sum_res$coefficients[4,4] < .05

# Stage 2: Binary event example
# Try running this code:
external_event <- rbinom(n = n, size = 1, prob = 0.5)
# Now insert it in `formal_model.R`, replacing the line "external_event <- rnorm(n = n)"
# Verify that the code still runs:
source("formal_model.R")

# Interaction effect example
# Try running this code
intrinsic_motivation <-
  0.56 * external_event +
  0.59 * needs +
  0.80 * external_event * needs +
  rnorm(n = n)
# Now insert it in `formal_model.R`
# Verify that the code still runs:
source("formal_model.R")

# Quadratic effect example
# Try running this code
intrinsic_motivation <-
  0.56 * external_event +
  0.59 * needs -
  0.70 * needs^2 +
  rnorm(n = n)
# Now insert it in `formal_model.R`
# Verify that the code still runs:
source("formal_model.R")
