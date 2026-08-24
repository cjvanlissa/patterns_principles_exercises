df <- readRDS("nguyen_2026.rds")
dependent_variable <- "ACM"
X <- model.matrix(as.formula(paste0(dependent_variable, "~.-1")), df)[, -1]
colnames(X) <- gsub(" ", "_", colnames(X))
Y <- df[[dependent_variable]]
train <- sample.int(nrow(X), size = round(.7*nrow(X)))
X_train <- X[train, ]
Y_train <- Y[train]
X_test <- X[-train, ]
Y_test <- Y[-train]

scale_these <- apply(X,2,function(x)length(unique(x)) > 2)
tmp <- scale(X_train[, scale_these])
means <- attr(tmp, "scaled:center")
sds <- attr(tmp, "scaled:scale")
X_train[, scale_these] <- tmp

X_test[, scale_these] <- sweep(X_test[, scale_these], 2, means, FUN = "-")
X_test[, scale_these] <- sweep(X_test[, scale_these], 2, sds, FUN = "/")

fold_id <- sample.int(10L, size = nrow(X_train), replace = TRUE)

# Lasso
library(glmnet)
res_cv <- glmnet::cv.glmnet(X_train, Y_train, foldid = fold_id)
res_lasso <- glmnet::glmnet(X_train, Y_train, lambda = res_cv$lambda.1se)

pred_lasso_test <- predict(res_cv,
                           newx = X_test,
                           s = "lambda.1se")
pred_lasso_train <- predict(res_cv,
                            newx = X_train,
                            s = "lambda.1se")

# Random Forest
library(mlr)
library(tuneRanger)
library(ranger)
reg_task <- mlr::makeRegrTask(data = data.frame(Y = Y_train, X_train), target = "Y", blocking = factor(fold_id))
# Tuning
res_tune_ranger <- tuneRanger::tuneRanger(reg_task, measure = list(mse), tune.parameters = c("mtry", "min.node.size"))
res_rf <- ranger::ranger(
  formula = Y~.,
  data = data.frame(Y = Y, X),
  mtry = res_tune_ranger$recommended.pars$mtry,
  importance = "permutation",
  min.node.size = res_tune_ranger$recommended.pars$min.node.size
)

pred_rf_test <- predict(res_tune_ranger$model$learner.model, data = data.frame(Y = Y_test, X_test))$predictions
pred_rf_train <- predict(res_tune_ranger$model$learner.model, data = data.frame(Y = Y_train, X_train))$predictions

# Regression tree
library(rpart)

tune_grid <- expand.grid(
  minbucket = as.integer(seq(2, nrow(X_train)/100, length.out = 10))
)

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

res_tree <- rpart(
  formula = Y ~ .,
  data = data.frame(Y = Y_train, X_train),
  method = "anova",
  control = do.call(rpart.control, args = as.list(tune_grid[which.min(colMeans(res_tune)), , drop = F]))
)

pred_tree_test <- predict(res_tree, newdata = data.frame(Y = Y_test, X_test))
pred_tree_train <- predict(res_tree, newdata = data.frame(Y = Y_train, X_train))

# Symbolic Regression
library(gramEvol)
ruleDef <- list(expr = grule(op(var, var)),
                op   = grule(`+`, `-`, `*`, `/`),
                var  = do.call(grule, args = as.list((sapply(c("n", colnames(X_train)), as.name)))),
                n    = gvrule(seq(-10,10,by=0.01)  #constants
                              )
  )

grammarDef <- CreateGrammar(ruleDef)


# Construct training environment
env_train <- new.env()
for(i in 1:ncol(X_train)){
  assign(colnames(X_train)[i], X_train[, i], envir = env_train)
}
assign("Y_train", Y_train, envir = env_train)

ge <- with(env_train, {
  SymRegFitFunc <- function(expr) {
    result <- eval(expr)
    if (any(is.nan(result)))
      return(Inf)
    return (mean((Y_train - result)^2))
  }
  GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.01, iterations = 10000, max.depth = 10)
})

pred_sr_train <- eval(ge$best$expressions, envir = env_train)

# Compute test error
env_test <- new.env()
for(i in 1:ncol(X_test)){
  assign(colnames(X_test)[i], X_test[, i], envir = env_test)
}
assign("Y_test", Y_test, envir = env_test)
pred_sr_test <- eval(ge$best$expressions, envir = env_test)

table_fits <- data.frame(
  method = c("LASSO", "Random Forest", "Regression Tree", "Symbolic regression"),
  r2_train = sapply(list(pred_lasso_train, pred_rf_train, pred_tree_train, pred_sr_train), rsq, obs = Y_train, mn = mean(Y_train)),
  r2_test = sapply(list(pred_lasso_test, pred_rf_test, pred_tree_test, pred_sr_test), rsq, obs = Y_test, mn = mean(Y_test))
  )

# Interpret LASSO
table_lasso_coef <- coef(res_lasso, s = "lambda.1se")
table_lasso_coef

# Interpret Random Forest
library(metaforest)
vi <- res_rf$variable.importance
vi <- sort(vi, decreasing = TRUE)
vi <- vi[vi > 0]
plot_vi <- VarImpPlot(res_rf)
plot_vi

plot_pdp <- PartialDependence(res_rf, data = data.frame(Y = Y, X), vars = names(vi))
plot(plot_pdp)

# Interpret Tree
library(rpart.plot)
prp(res_tree)

# Symbolic regression
ge
