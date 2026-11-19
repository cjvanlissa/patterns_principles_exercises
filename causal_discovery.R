# Causal discovery tutorial

# Setup
#| message: false
library(tidySEM)
library(ppcor)
library(qgraph)
library(pcalg)
library(Rgraphviz)
library(dagitty)
library(tpc)


# Helper function
edges_of <- function(fit) {
  # coerce the fit into an adjacancy matrix
  m <- as(fit, "amat")
  # store the variable names
  nms <- colnames(m)
  # initialize empty vector to collect edge strings
  out <- character(0)

  # loop over every ordered pair of variable names
  for (i in nms) for (j in nms) {
    # Skips the diagonal (since a variable has no edge to itself)
    if (i == j) next
    # pcalg convention: m[i, j] == 0 and m[j, i] == 1 means i -> j
    # Detects a directed edge
    if (m[j, i] == 1 && m[i, j] == 0) out <- c(out, paste(i, "->", j))
    # Detects a undirected edge
    if (m[i, j] == 1 && m[j, i] == 1 && i < j) out <- c(out, paste(i, "--", j))
  }
  out
}

# Chain
set.seed(6)
n <- 5000   # large, so that sampling noise does not obscure the pattern
a <- 0.6    # strength of every arrow, on a correlation scale

chain <- local({
  X <- rnorm(n)
  Z <- a * X + rnorm(n, sd = sqrt(1 - a^2))
  Y <- a * Z + rnorm(n, sd = sqrt(1 - a^2))
  data.frame(X = X, Z = Z, Y = Y)
})

cor(chain$X, chain$Y)                                  # related?
pcor.test(chain$X, chain$Y, chain$Z)$estimate          # still related given Z?

# Common cause
fork <- local({
  Z <- rnorm(n)
  X <- a * Z + rnorm(n, sd = sqrt(1 - a^2))
  Y <- a * Z + rnorm(n, sd = sqrt(1 - a^2))
  data.frame(X = X, Z = Z, Y = Y)
})

cor(fork$X, fork$Y)
pcor.test(fork$X, fork$Y, fork$Z)$estimate

# Common effect
collider <- local({
  X <- rnorm(n)
  Y <- rnorm(n)
  Z <- a * X + a * Y + rnorm(n, sd = sqrt(1 - 2 * a^2))
  data.frame(X = X, Z = Z, Y = Y)
})

cor(collider$X, collider$Y)
pcor.test(collider$X, collider$Y, collider$Z)$estimate

# Compare patterns
trace <- function(d) {
  c(r_XY = cor(d$X, d$Y),
    r_XY_given_Z = pcor.test(d$X, d$Y, d$Z)$estimate)
}

patterns <- data.frame(
  pattern = c("Chain:         X -> Z -> Y",
              "Common cause:  X <- Z -> Y",
              "Common effect: X -> Z <- Y"),
  rbind(trace(chain), trace(fork), trace(collider))
)
print(patterns, digits = 2, row.names = FALSE)

# Simulation setup
set.seed(1)
n <- 1000
vars <- c("AM", "TS", "BI", "BH")
alpha <- 0.10 # significance level of independence tests

# Run PC
run_pc <- function(data, alpha = 0.10) {
  pcalg::pc(suffStat = list(C = cor(data), n = nrow(data)),
     indepTest = gaussCItest,
     alpha     = alpha,
     labels    = names(data))
}

# World 1
world1 <- local({
  TS <- rnorm(n)
  AM <- rnorm(n)
  BI <- 0.6 * TS + 0.6 * AM + rnorm(n)
  BH <- 0.6 * BI + rnorm(n)
  data.frame(AM = AM, TS = TS, BI = BI, BH = BH)
})

fit1 <- run_pc(data = world1)
edges_of(fit1)
plot(fit1, main = "World 1: what PC recovered")

# World 2
world2 <- local({
  TS <- rnorm(n)
  AM <- 0.6 * TS + rnorm(n)
  BI <- 0.6 * AM + rnorm(n)
  BH <- 0.6 * BI + rnorm(n)
  data.frame(AM = AM, TS = TS, BI = BI, BH = BH)
})

fit2 <- run_pc(data = world2)
edges_of(fit2)
plot(fit2, main = "World 2: what PC recovered")

# Count compatible structures
n_structures <- function(fit) nrow(pcalg::pdag2allDags(as(fit, "matrix"))$dags)

n_structures(fit1)
n_structures(fit2)

# Load data
data_raw <- readRDS("zhao_2025.rds")
vars <- c("AM", "TS", "BI", "BH")
df <- data_raw[, vars] # select four variables for this tutorial

print(descriptives(df), digits = 2)
print(cor(df), digits = 2)

# Theory DAG
theory <- dagitty('dag {
  TS -> AM
  AM -> BH
  BI -> BH
}')

coordinates(theory) <- list(
  x = c(TS = 0, AM = 1, BI = 2, BH = 2),
  y = c(TS = 0, AM = 0, BI = 0, BH = 1)
)

plot(theory)

# Implied independencies
dagitty::impliedConditionalIndependencies(theory)

# Test predictions
round(dagitty::localTests(theory, df, type = "cis"), 3) # cis: linear conditional independence

# Theory equivalence class
theory_cpdag <- dagitty::equivalenceClass(theory)
theory_cpdag
plot(theory_cpdag)

# PC on real data
suffStat <- list(C = cor(df), n = nrow(df))

pc_fit <- pc(suffStat  = suffStat,
             indepTest = gaussCItest,
             alpha     = alpha,
             labels    = vars)

edges_of(pc_fit)
plot(pc_fit, main = "Structure inferred by PC") # bidrectional arrows imply that the arrow is unoriented (not a cyclic relationship!)

