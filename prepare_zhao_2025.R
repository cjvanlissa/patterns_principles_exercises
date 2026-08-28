# Dataset source and citation:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC11698333/#sec006
# Zhao K, Zhao Y, Xu W. Relationship between middle school students' academic stress and physical exercise behavior from the perspective of Self-Determination Theory: The chained mediation of motivation and intention. PLoS One. 2025 Jan 3;20(1):e0316599. doi: 10.1371/journal.pone.0316599. PMID: 39752352; PMCID: PMC11698333.
library(tidySEM)
library(readxl)
df <- readxl::read_xlsx("pone.0316599.s002.xlsx", 1)
df <- df[!is.na(df[[1]]), -1]

# Alternatively, a dataset can be loaded from an SPSS (.sav) file.
# The 'foreign' package provides functions for importing data stored in
# formats used by other statistical software packages.
# install.packages("foreign")
# library(foreign)
# df <- foreign::read.spss("zhao_2025.sav", to.data.frame = TRUE)

# Another alternative is to load data from a comma-separated values
# (.csv) file. Setting stringsAsFactors = TRUE converts character variables
# to factors during import.
# df <- read.csv("zhao_2025.csv", stringsAsFactors = TRUE)


# First, inspect descriptive statistics for the variables in the dataset.
# This is an important initial data-screening step because it helps us
# understand the structure of the data and identify features such as
# variable ranges, means, standard deviations, missing values, and the
# number of unique observed values.
table_desc <- tidySEM::descriptives(df)
table_desc

# Notice that all variables have initially been imported as numeric variables.
# This includes 'gender' and 'field'. Although these variables are represented
# by numbers in the raw dataset, the numbers are actually category codes rather
# than meaningful numerical quantities. These variables should therefore be
# represented as categorical variables rather than numeric variables.
#
# The analyses planned later in this workflow accept variables represented as
# either of the following R data types:
# * factor: used for categorical variables, where observations belong to
#   distinct categories rather than having a continuous numerical value;
# * numeric or integer: used for variables whose numerical values can be
#   treated quantitatively.
#
# If the dataset contains variables stored as character, Date, or ordered
# variables, they should therefore be recoded before proceeding. Depending on
# what the variable represents, either recode it into a factor or numeric
# variable, or remove it from the analysis dataset if it is not needed.
#
# Recode variables that represent categories as factors.
# Here we recode 'gender' and 'field' from their original numeric codes into
# factor variables. We also attach meaningful text labels to the numeric
# categories so that subsequent tables and output are easier to understand.
# The 'levels' argument specifies the original codes found in the dataset,
# while 'labels' specifies the corresponding category names that R should
# display instead.
df$Gender <- factor(df$Gender,
                    levels = c(1,2),
                    labels = c("M", "F"))

# Several variables in the dataset are individual questionnaire items that
# belong to broader multi-item scales. Here we create a named list that records
# which item variables belong to each scale.
#
# For example, the scale named TS consists of TS1 through TS4, while SOS
# consists of SOS1 through SOS4. Storing this information in a list allows the
# later code to perform the same psychometric procedures automatically for
# every scale rather than requiring separate code for each one.
#
# To prepare the scales list, I used:
# scales_list <- grep("\\w{2}\\d", names(df), value = TRUE)
# scalenames <- unique(gsub("\\d", "", scales_list))
# scales_list <- lapply(scalenames, grep, x = names(df), value = TRUE)
# names(scales_list) <- scalenames
#
# You can also edit it by hand:
scales_list <- list(
  TS = c("TS1", "TS2", "TS3", "TS4"),
  SOS = c("SOS1", "SOS2", "SOS3", "SOS4"),
  PS = c("PS1", "PS2", "PS3", "PS4", "PS5"),
  AM = c("AM1", "AM2", "AM3", "AM4", "AM5"),
  CM = c("CM1", "CM2", "CM3", "CM4"),
  BI = c("BI1", "BI2", "BI3"),
  BH = c("BH1", "BH2")
  )

# Psychometrics -----------------------------------------------------------

# Run the same psychometric analysis for each scale
psychmet <- lapply(names(scales_list), function(scal) {

  indicators <- scales_list[[scal]]
  df_tmp <- df[indicators]
  # Treat variables with fewer than 10 unique values as ordered categorical
  is_ordered <- sapply(df_tmp, function(x) length(unique(na.omit(x))) < 10)

  df_tmp[is_ordered] <- lapply(df_tmp[is_ordered], ordered)


  # Define a one-factor CFA model
  if(length(indicators) == 2){
    syntx <- paste(scal, "=~", paste("a*", indicators, collapse = " + "))
  } else {
    syntx <- paste(scal, "=~", paste(indicators, collapse = " + "))
  }
  # Fit CFA
  res <- lavaan::cfa(
    model = syntx,
    data = df_tmp,
    ordered = if(any(is_ordered)) names(df_tmp)[is_ordered] else NULL,
    std.lv = TRUE,
    auto.fix.first = FALSE
  )

  # Model fit
  fits <- tidySEM::table_fit(res)[, c("Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")]

  # Standardized factor loadings
  loadings <- lavaan::standardizedSolution(res)
  loadings <- loadings$est.std[loadings$op == "=~"]

  # Summary table
  tab <- data.frame(
    variable = scal,
    items = length(indicators),
    loading_min = min(loadings),
    loading_max = max(loadings),
    fits
  )

  # Composite reliability
  tab$comp_rel <- unlist(semTools::compRelSEM(res))

  # Dimensionality
  tab$n_fact <- psych::fa.parallel(df[, indicators, drop = FALSE], plot = FALSE)$nfact

  # Mean scale score
  scores <- rowMeans(df[, indicators, drop = FALSE], na.rm = TRUE)

  list(
    psychometrics = tab,
    scores = scores
  )
})

tab_psychometrics <- do.call(rbind, lapply(psychmet, `[[`, 1))

# Inspect the resulting table of psychometric properties for all scales.
# The table summarizes information such as the number of items, CFA model-fit
# statistics, and composite reliability. These results can be used to judge
# whether each proposed scale shows acceptable measurement properties before
# its scale score is used in the subsequent analysis.
tab_psychometrics

# For now, only pay attention to `comp_rel`: the composite reliability.
# This should be higher than .60 for all scales (otherwise, they are mostly
# measuring noise).
#
# Then, we extract the scale scores that were calculated for each participant.
# These scores summarize the responses to the individual questionnaire items
# belonging to a scale, producing one variable per construct that can be used
# in the later statistical or machine-learning analyses.
scale_scores <- data.frame(do.call(cbind, lapply(psychmet, `[[`, 2)))
names(scale_scores) <- tab_psychometrics$variable

# Construct the dataset that will ultimately be used for analysis.
# The original individual scale items are removed from this version of the
# dataset and are replaced by the newly calculated scale scores. Variables
# that were not members of any scale are retained from the original dataset.
# This creates a more compact analysis dataset containing the variables we
# want to carry forward into later stages of the workflow.
df_anal <- data.frame(df[, setdiff(names(df), unlist(scales_list))],
                      scale_scores)


# Here you can remove variables from Optionally restrict the analysis dataset to a specified set of variables.
# Only variables whose names appear in the vector below will be retained.
# Consequently, a variable can be excluded from the final analysis simply by
# leaving its name out of this list. This provides an explicit way to control
# which variables are passed on to the later analysis pipeline.
#
# For example, this code removes TS and SOS:
# df_anal <- df_anal[, setdiff(names(df_anal), c("TS", "SOS"))]

# If your data has missing values, you can try something like this:
# install.packages("missRanger")
# library(missRanger)
# df_anal <- missRanger(df_anal)

# Save the completed analysis dataset as an RDS file.
# RDS is R's native format for saving a single R object. The resulting file
# can later be loaded with readRDS(), allowing the data-cleaning and
# psychometric-processing steps above to remain separate from the subsequent
# analysis pipeline.
saveRDS(df_anal, "zhao_2025.rds")
