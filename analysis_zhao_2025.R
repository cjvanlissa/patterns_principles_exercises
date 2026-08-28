# ============================================================
# BASIC MACHINE LEARNING PIPELINE
# ============================================================
# This script:
#   1. Loads and prepares the data
#   2. Splits the data into training and test sets
#   3. Standardizes continuous predictors
#   4. Creates cross-validation folds
#   5. Fits four different prediction models:
#        - LASSO regression
#        - Random forest
#        - Regression tree
#        - Symbolic regression
#   6. Compares their predictive performance
#   7. Demonstrates different approaches to model interpretation
# ============================================================

# Set a seed to ensure reproducibility
set.seed(42)
# ------------------------------------------------------------
# 1. LOAD AND PREPARE THE DATA
# ------------------------------------------------------------

# Read the dataset from an RDS file.
# readRDS() restores an R object that was previously saved with saveRDS().
# If you want to use a different dataset, modify the script in
# `prepare_zhao_2025.R`.
df <- readRDS("zhao_2025.rds")

# The name of the outcome/dependent variable that we want to predict.
# If you want to use a different outcome in this dataset, or one from your own
# dataset, modify the name here.
dependent_variable <- "AM"

# Create the predictor matrix X.
#
# model.matrix() converts the predictors into a numeric design matrix.
# This is especially useful when some predictors are factors/categorical
# variables, because R automatically converts them into dummy variables.
#
# paste0() constructs a formula such as:
#   AM ~ .
#
# "." means "use all other variables as predictors."
#
X <- model.matrix(as.formula(paste0(dependent_variable, "~.")), df)[, -1]

# Replace spaces in predictor names with underscores because some later
# functions cannot handle spaces.
colnames(X) <- gsub(" ", "_", colnames(X))

# Extract the dependent/outcome variable as vector Y.
Y <- df[[dependent_variable]]

# ------------------------------------------------------------
# 2. CREATE TRAINING AND TEST SETS
# ------------------------------------------------------------

# Randomly select 70% of the observations for training.
#
# The training data are used to estimate/tune the models.
# The remaining 30% are held out as test data and are used to evaluate
# how well the fitted models generalize to previously unseen observations.
train <- sample.int(nrow(X), size = round(.7*nrow(X)))

# Predictor and outcome data for the training set.
X_train <- X[train, ]
Y_train <- Y[train]

# Predictor and outcome data for the test set.
X_test <- X[-train, ]
Y_test <- Y[-train]


# ------------------------------------------------------------
# 3. STANDARDIZE CONTINUOUS PREDICTORS
# ------------------------------------------------------------

# Identify variables that have more than two unique values.
#
# Variables with only two unique values are typically binary/dummy variables.
# These are left unchanged.
#
# Variables with more than two values are treated as continuous predictors
# and will be standardized.
scale_these <- apply(X,2,function(x)length(unique(x)) > 2)

# Standardize the selected variables in the TRAINING data.
#
# scale() subtracts each variable's mean and divides by its standard deviation:
#
#     standardized value = (x - mean) / standard deviation
#
# This gives continuous predictors approximately mean 0 and standard deviation 1.
tmp <- scale(X_train[, scale_these])

# Save the training-set means and standard deviations.
means <- attr(tmp, "scaled:center")
sds <- attr(tmp, "scaled:scale")

# Replace the original training variables with their standardized versions.
X_train[, scale_these] <- tmp

# Now, standardize the TEST data using the means and standard deviations
# calculated from the TRAINING data.
#
# We should not calculate new means/SDs using the test set because that
# would allow information from the test set to influence model preparation.

# First subtract the training-set means, then divide by the standard deviations.
X_test[, scale_these] <- sweep(X_test[, scale_these], 2, means, FUN = "-")
X_test[, scale_these] <- sweep(X_test[, scale_these], 2, sds, FUN = "/")

# ------------------------------------------------------------
# 4. CREATE CROSS-VALIDATION FOLDS
# ------------------------------------------------------------

# Assign each training observation to one of 10 cross-validation folds to tune
# model hyperparameters.
fold_id <- sample.int(10L, size = nrow(X_train), replace = TRUE)



# ============================================================
# 5. LASSO REGRESSION
# ============================================================

# LASSO is a linear regression method with L1 regularization.
#
# It penalizes large coefficients and can shrink some coefficients
# all the way to zero. Consequently, LASSO can perform both:
#   - regularization
#   - variable selection

library(glmnet)

# Use cross-validation to determine the amount of regularization.
#
# lambda controls the strength of the penalty:
#   larger lambda -> more shrinkage
#   smaller lambda -> less shrinkage
#
# foldid tells cv.glmnet() which observations belong to each
# cross-validation fold.
res_cv <- glmnet::cv.glmnet(X_train, Y_train, foldid = fold_id)

# Fit a LASSO model using lambda.1se.
#
# lambda.1se is the largest lambda whose cross-validation error is
# within one standard error of the minimum CV error.
#
# This usually produces a somewhat simpler/more strongly regularized
# model than choosing the lambda with the absolute minimum error.
res_lasso <- glmnet::glmnet(X_train, Y_train, lambda = res_cv$lambda.1se)


# Generate predictions for the held-out TEST observations.
pred_lasso_test <- predict(res_cv,
                           newx = X_test,
                           s = "lambda.1se")

# Generate predictions for the TRAINING observations.
#
# Comparing training and test performance can help diagnose overfitting.
pred_lasso_train <- predict(res_cv,
                            newx = X_train,
                            s = "lambda.1se")



# ============================================================
# 6. RANDOM FOREST
# ============================================================

# Random forests combine many regression trees.
#
# Individual trees may be unstable, but averaging predictions across
# many trees generally improves predictive performance and reduces variance.

library(mlr)
library(tuneRanger)
library(ranger)

# Create a regression task for the mlr/tuneRanger framework.
#
# Y is the outcome and all columns of X_train are predictors.
#
# blocking = factor(fold_id) tells the tuning procedure which
# cross-validation fold each observation belongs to.
reg_task <- mlr::makeRegrTask(data = data.frame(Y = Y_train, X_train), target = "Y", blocking = factor(fold_id))


# ------------------------------------------------------------
# Random forest hyperparameter tuning
# ------------------------------------------------------------
# Tune two important random-forest hyperparameters:
#
# mtry:
#   Number of predictors randomly considered as candidates at each split.
#
# min.node.size:
#   Minimum number of observations allowed in a terminal node.
#
# mse (mean squared error) is used to decide which parameter combination
# performs best during cross-validation.
res_tune_ranger <- tuneRanger::tuneRanger(reg_task, measure = list(mse), tune.parameters = c("mtry", "min.node.size"))


# Fit a random forest to the full training data using the recommended
# tuning parameters.
#
# importance = "permutation" requests permutation-based variable importance.
# A predictor is considered important if randomly shuffling it substantially
# worsens the model's predictions.
res_rf <- ranger::ranger(
  formula = Y~.,
  data = data.frame(Y = Y, X),
  mtry = res_tune_ranger$recommended.pars$mtry,
  importance = "permutation",
  min.node.size = res_tune_ranger$recommended.pars$min.node.size
)


# Generate random-forest predictions for the test observations.
pred_rf_test <- predict(res_tune_ranger$model$learner.model, data = data.frame(Y = Y_test, X_test))$predictions

# Generate predictions for the training observations.
pred_rf_train <- predict(res_tune_ranger$model$learner.model, data = data.frame(Y = Y_train, X_train))$predictions



# ============================================================
# 7. REGRESSION TREE
# ============================================================

# A regression tree repeatedly divides the predictor space into regions
# and assigns observations within each region a common predicted value.
#
# Trees are often easier to interpret than random forests, but an
# individual tree can be more prone to overfitting.

library(rpart)


# ------------------------------------------------------------
# Create possible values for the tree hyperparameter
# ------------------------------------------------------------

# minbucket controls the minimum number of observations that may be
# placed in a terminal ("leaf") node of the tree.
#
# Here we generate 10 candidate values that will be evaluated using
# cross-validation.
tune_grid <- expand.grid(
  minbucket = as.integer(seq(2, nrow(X_train)/100, length.out = 10))
)


# ------------------------------------------------------------
# Cross-validation for the regression tree
# ------------------------------------------------------------

# For every candidate minbucket value, estimate its prediction error
# across the cross-validation folds.
res_tune <- sapply(1:nrow(tune_grid), function(i){
  sapply(seq_along(unique(fold_id)), function(thisfold){
    f <- which(fold_id == thisfold)
    Args <- list(
      formula = quote(Y ~ .),
      data = data.frame(Y = Y_train, X_train)[-f, ],
      method = "anova",
      control = do.call(rpart.control, args = as.list(tune_grid[i, , drop = F]))
    )
    res_tree <- do.call(rpart, args = Args)
    preds <- predict(res_tree, newdata = data.frame(Y = Y_train, X_train)[f, ])
    mean((Y_train[f]-preds)^2)
  })
})


# ------------------------------------------------------------
# Fit the final regression tree
# ------------------------------------------------------------
res_tree <- rpart(
  formula = Y ~ .,
  data = data.frame(Y = Y_train, X_train),
  method = "anova",
  control = do.call(rpart.control, args = as.list(tune_grid[which.min(colMeans(res_tune)), , drop = F]))
)

# Generate predictions for the test set.
pred_tree_test <- predict(res_tree, newdata = data.frame(Y = Y_test, X_test))

# Generate predictions for the training set.
pred_tree_train <- predict(res_tree, newdata = data.frame(Y = Y_train, X_train))



# ============================================================
# 8. SYMBOLIC REGRESSION
# ============================================================

# Symbolic regression searches for an explicit mathematical expression
# that predicts the dependent variable.
#
# Unlike standard regression, we do not specify the exact functional
# form in advance. Instead, an evolutionary search procedure explores
# different mathematical expressions.
#
# gramEvol uses grammatical evolution to perform this search.

library(gramEvol)


# ------------------------------------------------------------
# Define the grammar of possible equations
# ------------------------------------------------------------

# The grammar determines which mathematical expressions the algorithm
# is allowed to construct.
#
# expr:
#   An expression consists of an operator applied to two variables/
#   expressions.
#
# op:
#   Permitted mathematical operations are +, -, *, and /.
#
# var:
#   Possible elements include numerical constants ("n") and all predictors.
#
# n:
#   Numerical constants between -10 and 10, in increments of 0.01.
grammarDef <- CreateGrammar(list(expr = grule(op(var, var)),
                op   = grule(`+`, `-`, `*`, `/`),
                var  = do.call(grule, args = as.list((sapply(c("n", colnames(X_train)), as.name)))),
                n    = gvrule(seq(-10,10,by=0.01)  #constants
                )
))


# ------------------------------------------------------------
# Construct the training environment
# ------------------------------------------------------------

# Symbolic regression needs an "environment" within which all of the variables
# are available as vectors.
# We therefore create an environment for training:
env_train <- new.env()

# Add every training predictor to the environment.
for(i in 1:ncol(X_train)){
  assign(colnames(X_train)[i], X_train[, i], envir = env_train)
}

# Add the training outcome to the same environment.
assign("Y_train", Y_train, envir = env_train)


# ------------------------------------------------------------
# Run grammatical evolution
# ------------------------------------------------------------

res_sr <- with(env_train, {
  SymRegFitFunc <- function(expr) {
    result <- eval(expr)
    if (any(is.nan(result)))
      return(Inf)
    return (mean((Y_train - result)^2))
  }
  GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.01, iterations = 10000, max.depth = 10)
})


# Evaluate the best mathematical expression found by the algorithm
# on the training data.
pred_sr_train <- eval(res_sr$best$expressions, envir = env_train)



# ------------------------------------------------------------
# Compute symbolic-regression predictions for the test data
# ------------------------------------------------------------

# Create another environment containing the TEST predictors.
env_test <- new.env()

for(i in 1:ncol(X_test)){
  assign(colnames(X_test)[i], X_test[, i], envir = env_test)
}

# Add the observed test outcome to the environment.
assign("Y_test", Y_test, envir = env_test)

# Evaluate the best expression on the test predictors.
pred_sr_test <- eval(res_sr$best$expressions, envir = env_test)



# ============================================================
# 9. COMPARE PREDICTIVE PERFORMANCE
# ============================================================

# Construct a table comparing the four models.
#
# R-squared is calculated separately for:
#
#   r2_train = model performance on observations used for training
#   r2_test  = model performance on previously unseen observations
#
# Test-set performance is generally the more important measure of
# predictive generalization.
#
# A large difference between training and test performance can indicate
# that a model is overfitting the training data.
rsq <- function(obs, preds, mn){
  tss <- sum((obs-mn)^2)
  rss <- sum((preds - obs) ^ 2)
  return(1 - rss/tss)
}
table_fits <- data.frame(
  method = c("LASSO", "Random Forest", "Regression Tree", "Symbolic regression"),

  # Calculate R-squared for predictions on the training data.
  r2_train = sapply(list(pred_lasso_train, pred_rf_train, pred_tree_train, pred_sr_train),
                    rsq,
                    obs = Y_train,
                    mn = mean(Y_train)),

  # Calculate R-squared for predictions on the held-out test data.
  r2_test = sapply(list(pred_lasso_test, pred_rf_test, pred_tree_test, pred_sr_test),
                   rsq,
                   obs = Y_test,
                   mn = mean(Y_test))
)
# Save it to a csv file
write.csv(table_fits, "table_fits.csv", row.names = FALSE)

# For model interpretation, pick the model with the highest r2_test -
# or alternatively, pick an *interpretable* model with an r2_test close to
# the highest r2_test (see the definition of a Rashomon set in Rudin, 2019).

# ============================================================
# 10. INTERPRET THE LASSO MODEL
# ============================================================

# Extract the estimated coefficients from the LASSO model.
#
# Because LASSO performs variable selection, some coefficients may
# have been shrunk to exactly zero.
#
# Non-zero coefficients indicate predictors retained by the model.
# Their signs indicate the direction of their relationship with Y,
# conditional on the other predictors in this linear model.
table_lasso_coef <- coef(res_lasso, s = "lambda.1se")

# Print the coefficient table.
table_lasso_coef
# Save it to a csv file
write.csv(as.matrix(table_lasso_coef), "table_lasso_coef.csv", row.names = TRUE)



# ============================================================
# 11. INTERPRET THE REGRESSION TREE
# ============================================================

library(rpart.plot)

# Plot the fitted regression tree (and store it to a png file).
#
# The diagram allows us to inspect:
#   - which variables are selected for splits
#   - the decision rules used by the model
#   - the hierarchical structure of those decisions
#   - predicted values in the terminal nodes
png("prp_tree.png")
prp(res_tree)
dev.off()


# ============================================================
# 12. INTERPRET THE RANDOM FOREST
# ============================================================

library(metaforest)

# Extract permutation-based variable importance scores.
#
# Larger values indicate variables whose information contributes
# more strongly to the random forest's predictive performance.
vi <- res_rf$variable.importance

# Sort predictors from most to least important.
vi <- sort(vi, decreasing = TRUE)

# Keep predictors with positive variable-importance values.
vi <- vi[vi > 0]


# ------------------------------------------------------------
# Variable importance plot
# ------------------------------------------------------------

# Plot the random forest's variable importance scores.
plot_vi <- VarImpPlot(res_rf)
# Save it to a png file
ggsave("plot_vi.png", plot_vi)
plot_vi



# ------------------------------------------------------------
# Partial dependence plots
# ------------------------------------------------------------

# Variable importance tells us WHICH predictors matter, but not HOW
# predictions change as those predictors change.
#
# Partial dependence plots estimate the average predicted relationship
# between each selected predictor and the outcome while averaging over
# the other predictors.
plot_pdp <- PartialDependence(res_rf,
                              data = data.frame(Y = Y, X),
                              vars = names(vi))
# Save it to a png file
ggsave("plot_pdp.png", plot_pdp)
plot(plot_pdp)


# ============================================================
# 13. INTERPRET THE SYMBOLIC REGRESSION MODEL
# ============================================================

# Print the grammatical-evolution result.
#
# Of particular interest is the best mathematical expression found
# during the evolutionary search. Unlike a random forest, the model
# can therefore be represented directly as an explicit equation.
res_sr
# Save it to a text fine
cat(as.character(res_sr$best$expressions), file = "sr_expression.txt")
