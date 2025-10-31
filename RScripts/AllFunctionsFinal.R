#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
#---------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                -------------
#--------------------                    Functions built for the analysis                  -----------------------------------# 
#--------------------               Authors: Laudine Carbuccia & Arthur Heim               -----------------------------------#    
#--------------------                               Final version                          -----------------------------------#  
#--------------------                               July 2025                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#


###### All functions used in the paper

#######################################################################################-
#######################################################################################-

##### glanceCustomFixest ####

#######################################################################################-
#######################################################################################-

## Function to get the mean of the DV in modelsummary with fixest
glance_custom.fixest <- function(x, ...) {
  dv <- insight::get_response(x)
  dv <- sprintf("%.2f", mean(dv, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}


#######################################################################################-
#######################################################################################-

#### check_controls ####

#######################################################################################-
#######################################################################################-

# useful for customised table to check if there are controls and indicate a checkmark in which case

check_controls <- function(variables, yes = "✓", no = "") {
  checkmate::assert_character(variables, min.len = 1)
  checkmate::assert_string(no)
  checkmate::assert_string(yes)
  reg <- paste0("^", paste(variables, collapse = "$|^"), "$")
  fun <- function(model) {
    est <- get_estimates(model)
    df <- if (all(variables %in% est$term)) yes else no
    df <- data.frame(Controls = df)
    return(df)
  }
  list("fun" = fun, "regex" = reg)
}


#######################################################################################-
#######################################################################################-

##### perform_pairwise_tests #####

#######################################################################################-
#######################################################################################-



# Function to perform pairwise tests and tidy results. Used for Figure 2.
perform_pairwise_tests <- function(model, het_var_name) {
  # Get names of coefficients
  coef_names <- names(coef(model))
  
  # Create custom contrast matrix that preserves coefficient names
  n_coef <- length(coef_names)
  n_contrasts <- choose(n_coef, 2)  # Number of pairwise comparisons
  
  # Initialize contrast matrix
  K_custom <- matrix(0, nrow = n_contrasts, ncol = n_coef)
  colnames(K_custom) <- coef_names
  
  # Create contrast names and fill matrix
  contrast_names <- character(n_contrasts)
  row_idx <- 1
  
  for (i in 1:(n_coef-1)) {
    for (j in (i+1):n_coef) {
      # Create contrast: coef_i - coef_j
      K_custom[row_idx, i] <- 1
      K_custom[row_idx, j] <- -1
      
      # Create meaningful contrast name
      contrast_names[row_idx] <- paste(coef_names[i], "-", coef_names[j])
      row_idx <- row_idx + 1
    }
  }
  
  rownames(K_custom) <- contrast_names
  
  # Perform tests
  glht_result <- glht(model, linfct = K_custom, alternative = "two.sided")
  
  # Get summary with Westfall adjustment
  summary_result <- summary(glht_result, test = adjusted("Westfall"))
  
  # Get confidence intervals
  confint_result <- confint(glht_result, adjusted(type = "Westfall"))
  
  # Tidy results
  tidy_summary <- tidy(summary_result)
  tidy_confint <- tidy(confint_result)
  
  # Combine results
  combined_results <- left_join(tidy_summary, tidy_confint, by = c("contrast","estimate"))
  
  # Add heterogeneity variable identifier
  combined_results$het_var <- het_var_name
  
  # Extract group1 and group2 from contrast names
  # The contrast format is now "full_coef_name1 - full_coef_name2"
  combined_results <- combined_results %>%
    mutate(
      # Split the contrast by " - " to get the two groups
      group1_full = str_trim(str_extract(contrast, "^[^-]+")),
      group2_full = str_trim(str_extract(contrast, "(?<= - ).*$"))
    ) %>%
    mutate(
      # Clean up group names by removing model prefix if present
      group1_full = str_remove(group1_full, "^model::"),
      group2_full = str_remove(group2_full, "^model::"),
      # Extract the model part (everything before the first ":")
      group1 = str_extract(group1_full, "^[^:]+"),
      group2 = str_extract(group2_full, "^[^:]+")
    ) %>%
    select(-group1_full, -group2_full)  # Remove temporary columns
  
  return(combined_results)
}


#######################################################################################-
#######################################################################################-

##### EstDoubleDebiased #####

#######################################################################################-
#######################################################################################-




EstDoubleDebiased <- function(Y = "ECSApp",                 # Outcome variable name
                              Z = "Z",                      # Treatment variable name
                              Cluster = "SubSampleStrata",  # Cluster variable name
                              FE = "SubSampleStrata",       # Fixed effects variable name
                              weights = "WeightPS",         # Weights variable name
                              SubSample = "T2-T1",          # Comparison group
                              DB = PostDB,                  # Main database
                              X = X.c,                      # Covariate database
                              exclude_patterns = c("educ2", "intend_use", "ecec_covering", "wave"), # Variables to exclude from selection
                              nfolds = 5,                   # Number of CV folds
                              alpha = 1,                    # Lasso penalty (1 = lasso, 0 = ridge)
                              nearZeroVar_threshold = 5,    # Threshold for removing near-zero variance variables
                              lambda_choice = "lambda.min", # Lambda selection: "lambda.min" or "lambda.1se"
                              k_folds_orthog = 2,           # Number of folds for cross-fitting (2 or 5 recommended)
                              double_lasso = TRUE,         # Use double lasso for post-lasso model
                              seed = 072025,                   # Random seed for reproducibility
                              verbose = TRUE                # Print progress messages
) {
  
  # DESCRIPTION:
  # This function implements double debiased post-lasso estimation for treatment effects.
  # It addresses selection bias in high-dimensional settings by using machine learning
  # to orthogonalize the treatment and outcome with respect to confounders.
  # The main component have been written by the authors
  # We used Claude.ia to add verbose and error checks to make the function more robust
  # All code output have been checks and all remaining errors are ours.
  
  # The function handles two scenarios which have been used to check models without interactions in the fixed effects.
  # The main estimates are based on the default parameters i.e. with FE and removing variables used to construct fixed effects. 
  # The function handles two scenarios:
  # 1. If FE is provided: Uses fixed effects as the sole design variables (unpenalized)
  #    and removes all variables matching exclude_patterns from covariates
  # 2. If FE is NULL: Creates design variables from interactions of exclude_patterns
  #    variables and uses them as unpenalized controls
  
  # Returns both double debiased estimates, post-lasso or
  # post-double-lasso estimates (for comparison). Uses cross-fitting to avoid overfitting bias.
  
  # Load required packages
  required_packages <- c("dplyr", "glmnet", "fixest", "caret", "stringr", "janitor","pacman")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (c("pacman") %in% missing_packages) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }
  
  if (length(missing_packages) > 0) {
    pacman::p_load(paste(missing_packages, collapse = ", "))
  }
  
  if (verbose) cat("Starting Double Debiased Post-Lasso estimation...\n")
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
    if (verbose) cat("Random seed set to:", seed, "\n")
  }
  
  # Input validation
  if (verbose) cat("Validating inputs...\n")
  
  # Check database exists
  if (!exists(deparse(substitute(DB)))) {
    stop("Database '", deparse(substitute(DB)), "' not found in environment")
  }
  
  if (!exists(deparse(substitute(X)))) {
    stop("Covariate database '", deparse(substitute(X)), "' not found in environment")
  }
  
  # Filter main database
  tryCatch({
    DBUse <- DB %>% filter(.data$SubSample == !!SubSample)
  }, error = function(e) {
    stop("Error filtering database: ", e$message, "\nCheck that 'SubSample' column exists and SubSample value is valid")
  })
  
  if (nrow(DBUse) == 0) {
    stop("No observations found for SubSample = '", SubSample, "'")
  }
  
  # Extract main variables
  Y_vec <- DBUse[[Y]]
  Z_vec <- DBUse[[Z]]
  cluster_vec <- DBUse[[Cluster]]
  FE_vec <- if (!is.null(FE)) DBUse[[FE]] else NULL
  weights_vec <- DBUse[[weights]]
  
  # Prepare covariate matrix 
  if (verbose) cat("Preparing covariate matrix...\n")
  
  XT <- X %>% filter(.data$Responded == 1 & .data$SubSample == !!SubSample)
  vars_to_remove <- c("SubSampleStrata", "SubSample", "ResponseId", "Responded")
  XT <- XT %>% select(-any_of(vars_to_remove))
  
  # Remove treatment variable from XT if present (we handle it separately)
  if ("Z" %in% colnames(XT)) {
    XT <- XT %>% select(-Z)
  }
  
  # Handle design variables based on FE specification
  if (!is.null(FE)) {
    # Case 1: FE provided - use fixed effects as design variables
    # Remove all variables matching exclude_patterns from covariates
    if (verbose) cat("Using fixed effects as design variables...\n")
    
    if (length(exclude_patterns) > 0) {
      pattern_regex <- paste(exclude_patterns, collapse = "|")
      exclude_cols <- which(str_detect(colnames(XT), pattern_regex))
      if (length(exclude_cols) > 0) {
        if (verbose) cat("Removing", length(exclude_cols), "variables matching exclude patterns from covariates\n")
        XT <- XT[, -exclude_cols, drop = FALSE]
      }
    }
    
    # Design variables are handled via fixed effects in the model
    X_design <- matrix(nrow = nrow(DBUse), ncol = 0)
    use_fixed_effects <- TRUE
    
  } else {
    # Case 2: No FE - create design variables from exclude_patterns interactions
    if (verbose) cat("Creating design variables from exclude_patterns interactions...\n")
    
    # Extract variables matching exclude_patterns for design matrix
    if (length(exclude_patterns) > 0) {
      pattern_regex <- paste(exclude_patterns, collapse = "|")
      
      # Find matching variables in DBUse (not XT)
      exclude_vars_in_db <- names(DBUse)[str_detect(names(DBUse), pattern_regex)]
      
      if (length(exclude_vars_in_db) > 0) {
        # Create design matrix from interactions of these variables
        design_data <- DBUse[, exclude_vars_in_db, drop = FALSE]
        
        if (ncol(design_data) == 1) {
          # Single variable - convert to factor and create dummy matrix
          X_design <- model.matrix(~ . - 1, design_data)
        } else {
          # Multiple variables - create interactions
          X_design <- tryCatch({
            # Try full interaction first
            model.matrix(~ . * . - 1, design_data)
          }, error = function(e) {
            if (verbose) cat("Full interaction failed, using additive model for design variables\n")
            model.matrix(~ . - 1, design_data)
          })
        }
        
        # Remove these variables from XT to avoid duplication
        exclude_cols_in_xt <- which(str_detect(colnames(XT), pattern_regex))
        if (length(exclude_cols_in_xt) > 0) {
          XT <- XT[, -exclude_cols_in_xt, drop = FALSE]
        }
      } else {
        if (verbose) cat("No variables found matching exclude_patterns in main database\n")
        X_design <- matrix(nrow = nrow(DBUse), ncol = 0)
      }
    } else {
      X_design <- matrix(nrow = nrow(DBUse), ncol = 0)
    }
    
    use_fixed_effects <- FALSE
  }
  
  # Create penalized variables matrix (all remaining covariates + their interactions)
  # First create main effects matrix from remaining variables
  if (ncol(XT) > 0) {
    X_other_main <- model.matrix(~0 + ., XT)
  } else {
    X_other_main <- matrix(nrow = nrow(DBUse), ncol = 0)
  }
  
  # Then create interactions among these variables (but NOT with design variables)
  if (ncol(X_other_main) > 1) {
    if (verbose) cat("Creating interactions among penalized variables...\n")
    X_other_interactions <- model.matrix(~0 + . * ., as.data.frame(X_other_main))
    
    # Remove the main effects that are already in X_other_main
    main_effect_names <- colnames(X_other_main)
    interaction_only_cols <- !colnames(X_other_interactions) %in% main_effect_names
    X_other_interactions <- X_other_interactions[, interaction_only_cols, drop = FALSE]
    
    # Combine main effects and interactions for penalized variables
    X_penalized <- cbind(X_other_main, X_other_interactions)
  } else {
    X_penalized <- X_other_main
  }
  
  # Remove near-zero variance variables from penalized set only
  if (ncol(X_penalized) > 0) {
    zero_var_cols <- nearZeroVar(X_penalized, uniqueCut = nearZeroVar_threshold, saveMetrics = FALSE)
    if (length(zero_var_cols) > 0) {
      if (verbose) cat("Removing", length(zero_var_cols), "near-zero variance variables from penalized set\n")
      X_penalized <- X_penalized[, -zero_var_cols, drop = FALSE]
    }
  }
  
  # Handle missing values
  if (use_fixed_effects) {
    complete_cases <- complete.cases(Y_vec, Z_vec, X_design, X_penalized, cluster_vec, FE_vec, weights_vec)
  } else {
    complete_cases <- complete.cases(Y_vec, Z_vec, X_design, X_penalized, cluster_vec, weights_vec)
  }
  
  if (sum(complete_cases) < length(complete_cases)) {
    n_missing <- sum(!complete_cases)
    if (verbose) cat("Removing", n_missing, "observations with missing values\n")
    
    Y_vec <- Y_vec[complete_cases]
    Z_vec <- Z_vec[complete_cases]
    X_design <- X_design[complete_cases, , drop = FALSE]
    X_penalized <- X_penalized[complete_cases, , drop = FALSE]
    cluster_vec <- cluster_vec[complete_cases]
    if (use_fixed_effects) FE_vec <- FE_vec[complete_cases]
    weights_vec <- weights_vec[complete_cases]
  }
  
  n_obs <- length(Y_vec)
  
  if (verbose) cat("Final sample: n =", n_obs, ", design vars =", ncol(X_design), 
                   ", penalized vars =", ncol(X_penalized), ", using FE =", use_fixed_effects, "\n")
  
  # ===== DOUBLE DEBIASED ESTIMATION =====
  
  # Combine design and penalized variables for the ML models
  if (ncol(X_design) > 0 && ncol(X_penalized) > 0) {
    X_full <- cbind(X_design, X_penalized)
  } else if (ncol(X_design) > 0) {
    X_full <- X_design
  } else if (ncol(X_penalized) > 0) {
    X_full <- X_penalized
  } else {
    X_full <- matrix(nrow = n_obs, ncol = 0)
  }
  
  # Create penalty factor: 0 for design variables, 1 for penalized variables
  penalty_factor <- c(rep(0, ncol(X_design)), rep(1, ncol(X_penalized)))
  
  # ===== CROSS-FITTING FOR ORTHOGONALIZATION =====
  if (verbose) cat("Using cross-fitting for orthogonalization...\n")
  
  # Helper function to create cluster-aware CV folds
  create_cluster_folds <- function(cluster_vec, nfolds, obs_indices = NULL) {
    if (is.null(obs_indices)) obs_indices <- 1:length(cluster_vec)
    
    cluster_subset <- cluster_vec[obs_indices]
    unique_clusters <- unique(cluster_subset)
    n_clusters <- length(unique_clusters)
    
    if (n_clusters >= nfolds) {
      # Cluster-aware folding
      cluster_folds <- sample(rep(1:nfolds, length.out = n_clusters))
      names(cluster_folds) <- unique_clusters
      fold_assignment <- cluster_folds[as.character(cluster_subset)]
      if (verbose && is.null(obs_indices)) cat("Using cluster-aware CV with", n_clusters, "clusters\n")
    } else {
      # Fall back to standard folding if too few clusters
      fold_assignment <- sample(rep(1:nfolds, length.out = length(obs_indices)))
      if (verbose && is.null(obs_indices)) cat("Too few clusters for cluster-aware CV, using standard CV\n")
    }
    
    return(fold_assignment)
  }
  
  # Create cross-fitting folds (cluster-aware)
  cf_folds <- create_cluster_folds(cluster_vec, k_folds_orthog)
  
  # Initialize vectors for cross-fitted predictions
  Y_residual <- numeric(n_obs)
  Z_residual <- numeric(n_obs)
  
  # Cross-fitting loop
  for (fold in 1:k_folds_orthog) {
    if (verbose) cat("Cross-fitting fold", fold, "of", k_folds_orthog, "\n")
    
    train_idx <- which(cf_folds != fold)
    test_idx <- which(cf_folds == fold)
    
    # ===== Step 1: Estimate outcome model on training fold =====
    # Combine design and penalized variables for training
    if (ncol(X_design) > 0 && ncol(X_penalized) > 0) {
      X_train_full <- cbind(X_design[train_idx, , drop = FALSE], 
                            X_penalized[train_idx, , drop = FALSE])
    } else if (ncol(X_design) > 0) {
      X_train_full <- X_design[train_idx, , drop = FALSE]
    } else if (ncol(X_penalized) > 0) {
      X_train_full <- X_penalized[train_idx, , drop = FALSE]
    } else {
      X_train_full <- matrix(nrow = length(train_idx), ncol = 0)
    }
    
    # Create penalty factor for this fold: 0 for design variables, 1 for others
    penalty_factor_fold <- c(rep(0, ncol(X_design)), rep(1, ncol(X_penalized)))
    
    if (ncol(X_train_full) > 0) {
      # Create cluster-aware CV folds for glmnet within this training fold
      train_cv_folds <- create_cluster_folds(cluster_vec, min(5, length(train_idx)), train_idx)
      
      # Outcome model: E[Y|X] (excluding treatment)
      cv_outcome_fold <- cv.glmnet(
        X_train_full, Y_vec[train_idx], 
        alpha = alpha, foldid = train_cv_folds,
        weights = weights_vec[train_idx],
        penalty.factor = penalty_factor_fold,
        parallel = FALSE
      )
      
      # Predict on test fold
      if (ncol(X_design) > 0 && ncol(X_penalized) > 0) {
        X_test_full <- cbind(X_design[test_idx, , drop = FALSE], 
                             X_penalized[test_idx, , drop = FALSE])
      } else if (ncol(X_design) > 0) {
        X_test_full <- X_design[test_idx, , drop = FALSE]
      } else if (ncol(X_penalized) > 0) {
        X_test_full <- X_penalized[test_idx, , drop = FALSE]
      } else {
        X_test_full <- matrix(nrow = length(test_idx), ncol = 0)
      }
      
      Y_pred <- predict(cv_outcome_fold, newx = X_test_full, s = lambda_choice)
      Y_residual[test_idx] <- Y_vec[test_idx] - as.vector(Y_pred)
    } else {
      # No covariates case
      Y_residual[test_idx] <- Y_vec[test_idx] - mean(Y_vec[train_idx])
    }
    
    # ===== Step 2: Estimate propensity score model on training fold =====
    if (ncol(X_train_full) > 0) {
      # Use the same CV folds for consistency
      train_cv_folds <- create_cluster_folds(cluster_vec, min(5, length(train_idx)), train_idx)
      
      # Propensity score model: E[Z|X]
      cv_treatment_fold <- cv.glmnet(
        X_train_full, Z_vec[train_idx], 
        alpha = alpha, foldid = train_cv_folds,
        weights = weights_vec[train_idx],
        family = "binomial",
        penalty.factor = penalty_factor_fold,
        parallel = FALSE
      )
      
      # Predict on test fold
      Z_pred <- predict(cv_treatment_fold, newx = X_test_full, s = lambda_choice, type = "response")
      Z_residual[test_idx] <- Z_vec[test_idx] - as.vector(Z_pred)
    } else {
      # No covariates case
      prop_score <- mean(Z_vec[train_idx])
      Z_residual[test_idx] <- Z_vec[test_idx] - prop_score
    }
  }
  
  # ===== Step 3: Debiased estimation =====
  if (verbose) cat("Computing debiased treatment effect estimate...\n")
  
  # Basic debiased estimator: regress residualized Y on residualized Z
  if (use_fixed_effects) {
    # With fixed effects - residualize with respect to FE as well
    fe_data <- data.frame(Y_residual, FE_vec)
    fe_model_y <- feols(Y_residual ~ 0 | FE_vec, data = fe_data)
    Y_residual_fe <- residuals(fe_model_y)
    
    fe_data_z <- data.frame(Z_residual, FE_vec)
    fe_model_z <- feols(Z_residual ~ 0 | FE_vec, data = fe_data_z)
    Z_residual_fe <- residuals(fe_model_z)
    
    debiased_data <- data.frame(
      Y_res = Y_residual_fe,
      Z_res = Z_residual_fe,
      cluster = cluster_vec,
      weights = weights_vec
    )
    
    debiased_model <- feols(Y_res ~ Z_res - 1, data = debiased_data,
                            cluster = ~cluster, weights = ~weights)
  } else {
    # Without fixed effects
    debiased_data <- data.frame(
      Y_res = Y_residual,
      Z_res = Z_residual,
      cluster = cluster_vec,
      weights = weights_vec
    )
    
    debiased_model <- feols(Y_res ~ Z_res - 1, data = debiased_data,
                            cluster = ~cluster, weights = ~weights)
  }
  
  # ===== Step 4: Estimate baseline model for comparison =====
  if (verbose) cat("Estimating baseline model (Y ~ Z | FE)...\n")
  
  if (use_fixed_effects) {
    # With fixed effects from FE variable
    baseline_data <- data.frame(
      Y = Y_vec,
      Z = Z_vec,
      FE = FE_vec,
      cluster = cluster_vec,
      weights = weights_vec
    )
    baseline_model <- feols(Y ~ Z | FE, data = baseline_data, 
                            cluster = ~cluster, weights = ~weights)
  } else {
    # Use design variables (interactions from exclude_patterns) as fixed effects
    if (ncol(X_design) > 0) {
      # Create fixed effects from design variables
      # Convert design matrix to factor for fixed effects
      design_factor <- apply(X_design, 1, function(x) paste(x, collapse = "_"))
      
      baseline_data <- data.frame(
        Y = Y_vec,
        Z = Z_vec,
        design_FE = as.factor(design_factor),
        cluster = cluster_vec,
        weights = weights_vec
      )
      baseline_model <- feols(Y ~ Z | design_FE, data = baseline_data, 
                              cluster = ~cluster, weights = ~weights)
    } else {
      # No design variables - just Y ~ Z
      baseline_data <- data.frame(
        Y = Y_vec,
        Z = Z_vec,
        cluster = cluster_vec,
        weights = weights_vec
      )
      baseline_model <- feols(Y ~ Z, data = baseline_data, 
                              cluster = ~cluster, weights = ~weights)
    }
  }
  
  # ===== Step 5: Estimate post-lasso or post-double-lasso for comparison =====
  if (double_lasso) {
    if (verbose) cat("Estimating post-double-lasso for comparison...\n")
    
    # Post-double-lasso: select variables from both outcome and treatment models
    if (ncol(X_penalized) > 0) {
      # Use only penalized variables for selection
      cv_outcome_select <- cv.glmnet(X_penalized, Y_vec, alpha = alpha, nfolds = nfolds,
                                     weights = weights_vec, parallel = FALSE)
      cv_treatment_select <- cv.glmnet(X_penalized, Z_vec, alpha = alpha, nfolds = nfolds,
                                       weights = weights_vec, family = "binomial", parallel = FALSE)
      
      selected_outcome <- which(as.vector(coef(cv_outcome_select, s = lambda_choice)[-1]) != 0)
      selected_treatment <- which(as.vector(coef(cv_treatment_select, s = lambda_choice)[-1]) != 0)
      selected_penalized_vars <- union(selected_outcome, selected_treatment)
      
      comparison_method <- "Post-Double-Lasso"
    } else {
      selected_penalized_vars <- integer(0)
      comparison_method <- "Post-Double-Lasso"
    }
  } else {
    if (verbose) cat("Estimating standard post-lasso for comparison...\n")
    
    # Standard post-lasso: select variables from outcome model only
    if (ncol(X_penalized) > 0) {
      cv_outcome_select <- cv.glmnet(X_penalized, Y_vec, alpha = alpha, nfolds = nfolds,
                                     weights = weights_vec, parallel = FALSE)
      selected_outcome <- which(as.vector(coef(cv_outcome_select, s = lambda_choice)[-1]) != 0)
      selected_penalized_vars <- selected_outcome
      
      comparison_method <- "Post-Lasso"
    } else {
      selected_penalized_vars <- integer(0)
      comparison_method <- "Post-Lasso"
    }
  }
  
  # Build the comparison model with selected variables
  if (length(selected_penalized_vars) > 0) {
    selected_X_penalized <- X_penalized[, selected_penalized_vars, drop = FALSE]
    
    # Combine design variables (always included) with selected penalized variables
    if (ncol(X_design) > 0) {
      selected_X_combined <- cbind(X_design, selected_X_penalized)
    } else {
      selected_X_combined <- selected_X_penalized
    }
    
    # Create clean variable names
    selected_var_names <- make.names(colnames(selected_X_combined))
    colnames(selected_X_combined) <- selected_var_names
    
    if (use_fixed_effects) {
      comparison_data <- data.frame(
        Y = Y_vec,
        Z = Z_vec,
        cluster = cluster_vec,
        FE = FE_vec,
        weights = weights_vec
      )
      # Add selected variables to the data frame
      for (i in seq_along(selected_var_names)) {
        comparison_data[[selected_var_names[i]]] <- selected_X_combined[, i]
      }
      
      # Create formula string with fixed effects
      formula_str <- paste("Y ~ Z +", paste(selected_var_names, collapse = " + "), "| FE")
      comparison_model <- feols(as.formula(formula_str), data = comparison_data,
                                cluster = ~cluster, weights = ~weights)
    } else {
      comparison_data <- data.frame(
        Y = Y_vec,
        Z = Z_vec,
        cluster = cluster_vec,
        weights = weights_vec
      )
      # Add selected variables to the data frame
      for (i in seq_along(selected_var_names)) {
        comparison_data[[selected_var_names[i]]] <- selected_X_combined[, i]
      }
      
      # Create formula string
      formula_str <- paste("Y ~ Z +", paste(selected_var_names, collapse = " + "))
      comparison_model <- feols(as.formula(formula_str), data = comparison_data,
                                cluster = ~cluster, weights = ~weights)
    }
  } else {
    # No penalized variables selected - use design variables only
    if (ncol(X_design) > 0) {
      design_var_names <- make.names(colnames(X_design))
      colnames(X_design) <- design_var_names
      
      if (use_fixed_effects) {
        comparison_data <- data.frame(
          Y = Y_vec,
          Z = Z_vec,
          FE = FE_vec,
          cluster = cluster_vec,
          weights = weights_vec
        )
        for (i in seq_along(design_var_names)) {
          comparison_data[[design_var_names[i]]] <- X_design[, i]
        }
        
        formula_str <- paste("Y ~ Z +", paste(design_var_names, collapse = " + "), "| FE")
        comparison_model <- feols(as.formula(formula_str), data = comparison_data,
                                  cluster = ~cluster, weights = ~weights)
      } else {
        comparison_data <- data.frame(
          Y = Y_vec,
          Z = Z_vec,
          cluster = cluster_vec,
          weights = weights_vec
        )
        for (i in seq_along(design_var_names)) {
          comparison_data[[design_var_names[i]]] <- X_design[, i]
        }
        
        formula_str <- paste("Y ~ Z +", paste(design_var_names, collapse = " + "))
        comparison_model <- feols(as.formula(formula_str), data = comparison_data,
                                  cluster = ~cluster, weights = ~weights)
      }
    } else {
      # No design variables and no selected penalized variables - use baseline model structure
      if (use_fixed_effects) {
        basic_data <- data.frame(Y = Y_vec, Z = Z_vec, FE = FE_vec, cluster = cluster_vec, weights = weights_vec)
        comparison_model <- feols(Y ~ Z | FE, data = basic_data, cluster = ~cluster, weights = ~weights)
      } else {
        basic_data <- data.frame(Y = Y_vec, Z = Z_vec, cluster = cluster_vec, weights = weights_vec)
        comparison_model <- feols(Y ~ Z, data = basic_data, cluster = ~cluster, weights = ~weights)
      }
    }
  }
  
  # ===== Prepare results =====
  if (verbose) cat("Preparing results...\n")
  
  # Extract key results
  debiased_coef <- coef(debiased_model)["Z_res"]
  debiased_se <- se(debiased_model)["Z_res"]
  
  baseline_coef <- coef(baseline_model)["Z"]
  baseline_se <- se(baseline_model)["Z"]
  
  comparison_coef <- coef(comparison_model)["Z"]
  comparison_se <- se(comparison_model)["Z"]
  
  results <- list(
    # Main results
    debiased_model = debiased_model,
    baseline_model = baseline_model,
    comparison_model = comparison_model,
    df_baseline = degrees_freedom(baseline_model,"t",cluster = ~cluster),
    df_debiased = degrees_freedom(debiased_model,"t",cluster = ~cluster),
    df_comparison = degrees_freedom(comparison_model,"t",cluster = ~cluster),
    # Treatment effect estimates
    treatment_effects = data.frame(
      Method = c("Double Debiased", "Baseline", comparison_method),
      Coefficient = c(debiased_coef, baseline_coef, comparison_coef),
      SE = c(debiased_se, baseline_se, comparison_se),
      t_stat = c(debiased_coef/debiased_se, baseline_coef/baseline_se, comparison_coef/comparison_se),
      p_value = c(2*pnorm(-abs(debiased_coef/debiased_se)), 
                  2*pnorm(-abs(baseline_coef/baseline_se)), 
                  2*pnorm(-abs(comparison_coef/comparison_se))),
      CI_lower = c(debiased_coef - 1.96*debiased_se, 
                   baseline_coef - 1.96*baseline_se, 
                   comparison_coef - 1.96*comparison_se),
      CI_upper = c(debiased_coef + 1.96*debiased_se, 
                   baseline_coef + 1.96*baseline_se, 
                   comparison_coef + 1.96*comparison_se),
      stringsAsFactors = FALSE
    ),
    
    # Residuals for further analysis
    residuals = list(
      Y_residual = Y_residual,
      Z_residual = Z_residual
    ),
    
    # Diagnostics
    diagnostics = list(
      n_obs = n_obs,
      n_design_vars = ncol(X_design),
      n_penalized_vars = ncol(X_penalized),
      n_selected_vars = if(exists("selected_penalized_vars")) length(selected_penalized_vars) else 0,
      k_folds_orthog = k_folds_orthog,
      fixed_effects_used = use_fixed_effects,
      double_lasso_used = double_lasso,
      efficiency_gain = (comparison_se^2 - debiased_se^2) / comparison_se^2
    ),
    
    call = match.call()
  )
  
  class(results) <- "DoubleDebiasedResults"
  
  if (verbose) {
    cat("Double Debiased estimation completed!\n")
    cat("Cross-fitting folds:", k_folds_orthog, "\n")
    cat("Comparison method:", comparison_method, "\n")
    cat("Efficiency gain:", round(results$diagnostics$efficiency_gain * 100, 2), "%\n")
    cat("Baseline ATE:", round(baseline_coef, 4), "(SE:", round(baseline_se, 4), ")\n")
    cat("Debiased ATE:", round(debiased_coef, 4), "(SE:", round(debiased_se, 4), ")\n")
    cat(comparison_method, "ATE:", round(comparison_coef, 4), "(SE:", round(comparison_se, 4), ")\n")
  }
  
  return(results)
}

# Print method for results
print.DoubleDebiasedResults <- function(x, ...) {
  cat("Double Debiased Post-Lasso Results\n")
  cat("==================================\n\n")
  
  cat("Sample size:", x$diagnostics$n_obs, "\n")
  cat("Design variables (unpenalized):", x$diagnostics$n_design_vars, "\n")
  cat("Penalized variables:", x$diagnostics$n_penalized_vars, "\n")
  cat("Selected variables:", x$diagnostics$n_selected_vars, "\n")
  cat("Cross-fitting folds:", x$diagnostics$k_folds_orthog, "\n")
  cat("Fixed effects used:", x$diagnostics$fixed_effects_used, "\n")
  cat("Double lasso used:", x$diagnostics$double_lasso_used, "\n\n")
  
  cat("Treatment Effect Estimates:\n")
  print(x$treatment_effects, row.names = FALSE, digits = 4)
  
  cat("\nEfficiency gain from debiasing:", 
      round(x$diagnostics$efficiency_gain * 100, 2), "%\n")
}



#######################################################################################-
#######################################################################################-

##### EstPostLasso #####

#######################################################################################-
#######################################################################################-

# Function requires package glmnet, fixest and estimatr



EstPostLasso <- function(Y="ECSApp",                 # Outcome as string
                         Z="Z",                      # Treatment variable as sting
                         Cluster= "SubSampleStrata", # Clusters 
                         FE = "SubSampleStrata"    , # Fixed effects
                         weights="WeightPS",         # Weights used in the ITT estimates
                         SubSample="T2-T1",          # Comparison group
                         DB=PostDB,                  # Database
                         X=X.c                       # X database (must be able to match with DB)
){  
  
  # get the database  
  DBUse <- {{DB}} %>% filter(SubSample=={{SubSample}}) 
  
  # Outcome
  Y <- DBUse[[{{Y}}]]  
  # treatment
  Z <- DBUse[[{{Z}}]]    
  # Cluster
  cluster <-  DBUse[[{{Cluster}}]]  
  # fixed effects
  FE <- DBUse[[{{Cluster}}]]  
  
  # Weights
  Weights <- DBUse[[{{weights}}]]  
  
  # New X matrix
  XT <- {{X}} %>% filter(Responded==1 & SubSample=={{SubSample}}) 
  
  
 
  # remove the variables we don't want, and add dummies for blocks and treatment to this matrix.
  XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
    bind_cols(model.matrix(~0+HighLowECECBaseline*Educ2*IntendUse*Which_wave+Z,DBUse))
  
  # put it as a matrix and allow every possible interactions (the number of columns gets crazy high)
  ModelMatrix <- model.matrix(~0+.*.,XT) 
  
  # We just don't want 
  ModelMatrix <- ModelMatrix[,-c(which(colnames(ModelMatrix)=="Z"))]
  
  # remove variables with less than 5% of distinct values (almost 0 variance variable which ends-up colinear)
  ZeroVar <- ModelMatrix %>% nearZeroVar(uniqueCut = 5) 
  ModelMatrix <- ModelMatrix[,-c(ZeroVar)]
  
  # Run the basic estimate
  # We use fixest::feols for the fixest effect. we put the variables in a database as this function requires a dataframe as input
  DB.FE.Reg <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  basicModel <- feols(Y~Z|FE,data=DB.FE.Reg,cluster = ~cluster,weights = ~weights)
  
  ## now, let's run a cross validation 
  # Cross validation to identify the optimal lambda (may take some time to converge)
  cv_Lasso2 <- cv.glmnet(ModelMatrix,Y , alpha = 1,nfolds = 10)
  
  # get the best lambda
  bestlam <- cv_Lasso2$lambda.min
  
  # Now, we can run the lasso model to pick the right covariate using the optimal lambda
  Lasso_reg2 <- glmnet(ModelMatrix, Y, alpha = 1, lambda=bestlam)
  
  DB.X <- as.data.frame(ModelMatrix[,c(which(Lasso_reg2$beta>0|Lasso_reg2$beta<0))])
  DB.Reg <- bind_cols(DB.FE.Reg,DB.X) %>% janitor::clean_names() %>% rename(Z=z,FE=fe)
  DB.FE.Xc <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  # Then, simply run the regression of the outcome on the treatment and the variables selected by lasso. 
  # lm_lin conviniently accept that and will even make sure that all variables are centred to ensure
  # a treatment effect interpretation.
  # post_lasso <- lm_robust(Y~Z+ModelMatrix[,c(which(Lasso_reg2$beta>0))],cluster=cluster,weights = Weights) 
  # Get all column names except Y, Z, and FE
  regressors <- setdiff(names(DB.Reg), c("y", "z", "FE","cluster","weights"))
  
  # do not get the design variable
  regressors <- regressors[str_detect(regressors,"educ2|intend_use|ecec_covering|wave")==FALSE]
  
  # Create the formula as a string
  formula_str <- paste("y ~ Z +", paste(regressors, collapse = " + "), "| FE")
  
  # Run the model
  post_lasso <- feols(as.formula(formula_str),data=DB.Reg,cluster = ~cluster,weights = ~weights)
  
  #post_lasso <- feols(Y~Z+.|FE,data=DB.Reg)
  #
  ITT_postLasso <- lm_lin(Y~Z,~ModelMatrix[,which((Lasso_reg2$beta>0 |Lasso_reg2$beta<0) & str_detect(rownames(Lasso_reg2$beta),":Z")==FALSE)],cluster=cluster,weights = Weights) 
  
  
  
  return(list("Basic Model"=basicModel,"Post Lasso"=post_lasso,"ITT Post lasso"=ITT_postLasso,"Lambda"=bestlam,"Lasso"=Lasso_reg2))
  
}

#test <- EstPostLasso()


EstPostLassolm_robust <- function(Y="ECSApp",                 # Outcome as string
                                  Z="Z",                      # Treatment variable as sting
                                  Cluster= "SubSampleStrata", # Clusters 
                                  FE = "SubSampleStrata"    , # Fixed effects
                                  weights="WeightPS",         # Weights used in the ITT estimates
                                  SubSample="T2-T1",          # Comparison group
                                  DB=PostDB,                  # Database
                                  X=X.c                       # X database (must be able to match with DB)
){  
  
  # get the database  
  DBUse <- {{DB}} %>% filter(SubSample=={{SubSample}}) 
  
  # Outcome
  Y <- DBUse[[{{Y}}]]  
  # treatment
  Z <- DBUse[[{{Z}}]]    
  # Cluster
  cluster <-  DBUse[[{{Cluster}}]]  
  # fixed effects
  FE <- DBUse[[{{Cluster}}]]  
  
  # Weights
  Weights <- DBUse[[{{weights}}]]  
  
  # New X matrix
  XT <- {{X}} %>% filter(Responded==1 & SubSample=={{SubSample}}) 
  
  
  ## We train a lasso to predict outcomes, so we want it to be able to pick the treated x covariate values that are high and so on.
  ## Therefore, the X matrix contains the treatment, and we want to interact the treatment with the rest.
  ## 
  
  # remove the variables we don't want, and add dummies for blocks and treatment to this matrix.
  XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
    bind_cols(model.matrix(~0+HighLowECECBaseline*Educ2*IntendUse*Which_wave,DBUse))
  
  # put it as a matrix and allow every possible interactions (the number of columns gets crazy high)
  ModelMatrix <- model.matrix(~0+.*.,XT) 
  
  # We just don't want 
  # ModelMatrix <- ModelMatrix[,-c(which(colnames(ModelMatrix)=="Z"))]
  
  # Run the basic estimate
  # We use fixest::feols for the fixest effect. we put the variables in a database as this function requires a dataframe as input
  DB.FE.Reg <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  basicModel <- feols(Y~Z|FE,data=DB.FE.Reg,cluster = ~cluster,weights = ~weights)
  
  
  ## now, let's run a cross validation 
  # Cross validation to identify the optimal lambda (may take some time to converge)
  cv_Lasso2 <- cv.glmnet(ModelMatrix,Y , alpha = 1,nfolds = 5)
  
  # get the best lambda
  bestlam <- cv_Lasso2$lambda.min
  
  # Now, we can run the lasso model to pick the right covariate using the optimal lambda
  Lasso_reg2 <- glmnet(ModelMatrix, Y, alpha = 1, lambda=bestlam)
  
  
  # Then, simply run the regression of the outcome on the treatment and the variables selected by lasso. 
  # lm_lin conviniently accept that and will even make sure that all variables are centred to ensure
  # a treatment effect interpretation.
  post_lasso <- lm_robust(Y~Z+ModelMatrix[,c(which(Lasso_reg2$beta>0))],cluster=cluster,weights = Weights) 
  
  #
  ITT_postLasso <- lm_lin(Y~Z,~ModelMatrix[,which(Lasso_reg2$beta>0 & str_detect(rownames(Lasso_reg2$beta),":Z")==FALSE)],cluster=cluster,weights = Weights) 
  
  
  
  return(list("Basic Model"=basicModel,"Post Lasso"=post_lasso,"ITT Post lasso"=ITT_postLasso,"Lambda"=bestlam,"Lasso"=Lasso_reg2))
  
}

#######################################################################################-
#######################################################################################-

##### ITTSimultaneous ####

#######################################################################################-
#######################################################################################-


# Function to compute average ITT with adjustment for multiple testing (choice of correction method as parameter)

ITTSimultaneous <- function(Y = "UseCreche",
                            treat = "Z",
                            DB = PostDB,
                            Correction = "Westfall",
                            sided.test = "two.sided",
                            weights = "WeightPS") {
  # Supported Correction methods include:
  # 'single-step', 'Shaffer', 'Westfall', 'free', 'holm', 'hochberg', 
  # 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none'
  # Supported sided.test values: 'two.sided', 'less', 'greater'
  
  #----------------------#
  # 1. Prepare the data
  #----------------------#
  DBInside <- DB
  DBInside$Y <- DBInside[[Y]]
  DBInside$Z <- DBInside[[treat]]
  
  # weights : initialized as 1 if no weights given
  if (weights==""){
    DBInside$w=1  
  }else{
    DBInside$w <- DBInside[[{{weights}}]]  
  }
  
  #----------------------#
  # 2. Estimate main model with fixest
  #----------------------#
  model <- feols(
    Y ~ i(Z, SubSample, ref = 0) | SubSampleStrata,
    data = DBInside,
    cluster = ~StrataWave,
    weights = ~w
  )
  
  #----------------------#
  # 3. Build hypothesis matrix (K) for glht
  # Creates an identity matrix where each row tests one coefficient against zero (H₀: βᵢ = 0).
  #----------------------#
  coef_names <- names(coef(model))
  K <- diag(length(coef_names))
  rownames(K) <- coef_names
  colnames(K) <- coef_names
  
  #----------------------#
  # 4. Run glht for simultaneous inference
  #----------------------#
  glht.model <- glht(
    model = model,
    linfct = K,
    alternative = sided.test
  )
  
  # Get adjusted p-values and simultaneous confidence intervals
  tidyglht <- left_join(
    tidy(glht.model, test = adjusted(type = Correction)),
    tidy(confint(glht.model, adjusted(type = Correction)))
  )
  
  #----------------------#
  # 5. Format tidy results for estimates
  #----------------------#
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),
                          tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("Z","term","Var"),sep="::") %>% mutate(term=Var,
                                                           Var=str_remove_all(Var,"MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )
  # Get degree of freedom of t-tests
  
  t.DF <- degrees_freedom(model,type="t",cluster = ~SubSampleStrata)
  
  #----------------------#
  # 6. Control group mean (via OLS trick)
  #----------------------#
  ControlMean <- feols(
    ZO * Y ~ i(ZO, SubSample, ref = 0) | SubSampleStrata,
    data = DBInside %>% mutate(ZO = 1 - Z),
    cluster = ~StrataWave
  )
  
  # joint significance test
  Glht.ControlMean <- glht(ControlMean) 
  
  # get the results in a tidy data frame
  tidy.Glht.ControlMean <- left_join(tidy(Glht.ControlMean),tidy(confint(Glht.ControlMean)))
  
  # tidy the model 
  tidy.ControlMean <- tidy(ControlMean) %>% bind_cols(.,confint(ControlMean)[1],
                                                      confint(ControlMean)[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.ControlMean,by=c("term"="contrast","estimate","std.error")) %>% 
    mutate(term=str_remove_all(term,"ZO::1:|\\(|\\)|SubSample::"))
  
  #----------------------#
  # 7. Joint significance test
  #Tests the global null hypothesis that all treatment effects are jointly zero using a chi-squared test.
  #----------------------#
  ChisQTest <- summary(glht.model, test = Chisqtest())
  
  #----------------------#
  # 8. Assemble final output
  #----------------------#
  fullModel <- list(
    tidy = tidy.final %>%
      bind_rows(
        tidy.ControlMean %>%
          filter(str_detect(term, "T2-C")) %>%
          mutate(term = "Control mean", across(c(adj.p.value, p.value), ~NA))
      ),
    glance = get_gof(model) %>%
      bind_cols("Fixed effects" = "X") %>%
      bind_cols(t(c(
        "DF t-test" = t.DF,
        "Chi 2" = ChisQTest$test$SSH,
        "DF Chi2" = ChisQTest$test$df[[1]],
        "P-value" = ChisQTest$test$pvalue
      )))
  )
  class(fullModel) <- "modelsummary_list"
  
  return(list(
    Estimation = model,
    Tidy = tidy.final,
    Correction = Correction,
    ModelSummary = fullModel
  ))
}


#######################################################################################-
#######################################################################################-

#### ITTDif ####

#######################################################################################-
#######################################################################################-


# this function is the former version of ITTSimultaneous and do not compute the mean of the control group inside.
# It is therefore useful if we want to trick it into computing control means for instance.
ITTDif <- function(Y="UseCreche",
                   DB=PostDB,
                   Correction="Westfall"
){
  #Data
  DBInside <- DB 
  DBInside$Y <- DBInside[[{{Y}}]]  
  
  #model  
  model <- feols(Y~i(Z,SubSample,ref=0)|SubSampleStrata,DBInside,cluster = ~StrataWave,weights = ~WeightPS)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("Z","term","Var"),sep="::") %>% mutate(term=str_remove_all(term,"SubSample"),
                                                           Var=str_remove_all(Var,"MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}}))    
}


#######################################################################################-
#######################################################################################-

#### LATESimultaneous ####

#######################################################################################-
#######################################################################################-

# this function is essentially the same as ITTSimultaneous, however it computes the LATE with TSLS instead.
# Note that it won't work for comparisons with T1 since we don't have the first stage.
#This function should therefore be used with PostDBT2
LATESimultaneous <- function(Y="UseCreche",
                             DB=PostDBT2,
                             Correction="Westfall",
                             weights="WeightPS"
){
  #Data
  DBInside <- DB 
  DBInside$Y <- DBInside[[{{Y}}]]  
  DBInside$w <- DBInside[[{{weights}}]]
  
  if (weights==""){
    DBInside$w=1  
  }
  
  #model  
  model <- feols(Y~1|SubSampleStrata|D:SubSample~Z.c:SubSample,DBInside,cluster = ~StrataWave,weights = ~w)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("D","term"),sep=":") %>% mutate(term=str_remove_all(term,"SubSample"))
  
  
  # Compute response rate in the comparison group
  # Compliers' missing potential outcome following Abadie 2003 and the trick with TSLS similar to the previous one
  Y0Compliers <- feols(D0*Y~1|SubSampleStrata|D0:SubSample~Z.c:SubSample,DBInside %>% mutate(ZO=1-Z,D0=1-D),cluster = ~StrataWave)
  
  
  # joint significance test
  Glht.ControlMean <- glht(Y0Compliers) 
  
  # get the results in a tidy data frame
  tidy.Glht.ControlMean <- left_join(tidy(Glht.ControlMean),tidy(confint(Glht.ControlMean)))
  
  # tidy the model 
  tidy.ControlMean <- tidy(Y0Compliers) %>% bind_cols(.,confint(Y0Compliers)[1],
                                                      confint(Y0Compliers)[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.ControlMean,by=c("term"="contrast","estimate","std.error")) %>% 
    mutate(term=str_remove_all(term,"ZO::1:|\\(|\\)|SubSample"))
  
  
  ### prepare for model summary
  
  ChisQTest <- glht(model) %>% summary(.,test=Chisqtest())
  
  fullModel <- list(tidy=tidy.final %>% 
                      bind_rows(.,tidy.ControlMean %>% filter(str_detect(term,"T2-T1")) %>% 
                                  mutate(term="Avg. cfct.") %>% 
                                  mutate_at(vars(adj.p.value,p.value),~NA)), # list with tidy containing the dataframe with the estimates, # list with tidy containing the dataframe with the estimates
                    glance=get_gof(model) %>%  # statistics of the model
                      bind_cols(.,"Fixed effects"="X") %>% 
                      bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                      "DF" = ChisQTest$test$df[[1]],
                                      "P-value"=  ChisQTest$test$pvalue)
                      )))
  class(fullModel) <- "modelsummary_list"   # define the class
  
  
  
  
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}},"ModelSummary"=fullModel))    
}


#######################################################################################-
#######################################################################################-

#### GroupHeterogeneityFn ####

#######################################################################################-
#######################################################################################-

# This function estimates either the conditional ITT or conditional LATE for a factor variable
# defined as the parameter Heterogeneity.  

GroupHeterogeneityFn <- function(DB = PostDBT2,             #Database
                                 Outcome = "UseCreche",    # Which outcome 
                                 Heterogeneity = "Educ2",  # Which heterogeneity variable
                                 ctrl.var = c("Act3","SingleMum","FrenchYNBaseline","Dep"), # NOT USED FOR NOW
                                 ITT = TRUE,               # ITT = TRUE -> OLS, ITT=FALSE -> TSLS Conditional LATE
                                 Weights = "WeightPS",     # Propensity score weights by default
                                 clusters = "StrataWave"   # Cluster variable
){
  
  DBInside <- {{DB}}
  
  #get the outcome
  DBInside$Y <- DBInside[[{{Outcome}}]]
  
  #get the weight
  DBInside$w <- DBInside[[{{Weights}}]]
  
  #Create heterogeneous variable
  DBInside$Het <- DBInside[[{{Heterogeneity}}]]
  
  # clusters
  DBInside$Clust <- DBInside[[{{clusters}}]]
  
  
  
  
  # Check if variable in the blocking variables
  
  InteractFE <- ({{Heterogeneity}} %in% c("HighLowECEC","Educ2","IntendUse","Which_wave"))
  
  if(InteractFE==TRUE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)  
    }
    else{
      model <- feols(Y~1|StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      # Get the F stat of the first stage
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
    }
  }
  
  if(InteractFE==FALSE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|Het^StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)  
    }
    else{
      model <- feols(Y~1|Het^StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
      
    }
  }
  
  # Tidy Database of the model
  
  M0 <- modelplot(model,draw=FALSE) %>% 
    separate(term,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het")) %>% 
    rename("point.conf.low"="conf.low","point.conf.high"="conf.high")
  
  
  ## Joint hypothesis testing for each value of the heterogeneity variable
  GLH <-c()
  temp <- c()
  HetVal <- unique(DBInside$Het) %>% as.character()
  
  
  for (value in HetVal){
    temp <- glht(model,
                 paste(paste("`",
                             names(model$coefficients)[
                               str_detect(names(model$coefficients),{{value}})],
                             "`= 0",sep="")))
    
    GLH <- bind_rows(GLH,left_join(tidy(temp),tidy(confint(temp))) %>% mutate(Het=value))
  } 
  
  # Clean names and labels
  GLH <- GLH %>% separate(contrast,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het"))
  
  # join with MO
  GLH <- left_join(GLH,M0 %>% select(-c(estimate,std.error)))
  
  #Prepare Gof for the model
  
  Gof <- get_gof(model)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof <- Gof %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  #%>% bind_cols(.,as.data.frame(t(Fstats)))
  
  
  ForModelSummary <- list(tidy=GLH,
                          glance=Gof
  )
  
  class(ForModelSummary) <- "modelsummary_list"   # define the class
  
  
  return(list("Estimation"=model,"Tidy"=GLH,"ModelSummary"=ForModelSummary))    
}


#######################################################################################-
#######################################################################################-

#### GroupHeterogeneityFnCTRL ####

#######################################################################################-
#######################################################################################-

# This function also compute the means in the contnrol group and adds it to the output 
# as both a model, and a list for modelsummary 

GroupHeterogeneityFnCTRL <- function(DB = PostDBT2,             #Database
                                     Outcome = "UseCreche",    # Which outcome 
                                     Heterogeneity = "Educ2",  # Which heterogeneity variable
                                     ITT = TRUE,               # ITT = TRUE -> OLS, ITT=FALSE -> TSLS Conditional LATE
                                     Weights = "WeightPS",     # Propensity score weights by default
                                     clusters = "StrataWave",   # Cluster variable
                                     Correction="Westfall"
){
  
  #Correction should be one of “single-step”, “Shaffer”, “Westfall”, “free”, “holm”, “hochberg”, “hommel”, “bonferroni”, “BH”, “BY”, “fdr”, “none”  
  DBInside <- {{DB}}
  
  #get the outcome
  DBInside$Y <- DBInside[[{{Outcome}}]]
  
  #get the weight
  DBInside$w <- DBInside[[{{Weights}}]]
  
  #Create heterogeneous variable
  DBInside$Het <- DBInside[[{{Heterogeneity}}]]
  
  # clusters
  DBInside$Clust <- DBInside[[{{clusters}}]]
  # Check if variable in the blocking variables
  
  InteractFE <- ({{Heterogeneity}} %in% c("HighLowECEC","Educ2","IntendUse","Which_wave"))
  
  if(InteractFE==TRUE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)
      model0 <- feols(Y~Z.c:SubSample:Het|StrataWave^SubSample,DBInside %>% mutate(Y=(1-Z)*Y,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
    }
    else{
      model <- feols(Y~1|StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      model0 <- feols(Y~1|StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside %>% mutate(Y=(1-D)*Y,D=1-D,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
      # Get the F stat of the first stage
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
    }
  }
  
  if(InteractFE==FALSE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|Het^StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)  
      model0 <- feols(Y~Z.c:SubSample:Het|Het^StrataWave^SubSample,DBInside %>% mutate(Y=(1-Z)*Y,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
    }
    else{
      model <- feols(Y~1|Het^StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      model0 <- feols(Y~1|Het^StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside %>% mutate(Y=(1-D)*Y,D=1-D,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
      
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
      
    }
  }
  
  # Tidy Database of the model
  
  M0 <- modelplot(model,draw=FALSE) %>% 
    separate(term,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het")) %>% 
    rename("point.conf.low"="conf.low","point.conf.high"="conf.high")
  
  
  ## Joint hypothesis testing for each value of the heterogeneity variable
  GLH <-c()
  temp <- c()
  HetVal <- unique(DBInside$Het) %>% as.character()
  
  
  for (value in HetVal){
    temp <- glht(model,
                 paste(paste("`",
                             names(model$coefficients)[
                               str_detect(names(model$coefficients),{{value}})],
                             "`= 0",sep="")))
    sum.temp <- summary(temp,adjusted(type={{Correction}}))
    CI.temp <- confint(temp)
    # GLH <- bind_rows(GLH,left_join(tidy(temp),
    #                               tidy(confint(temp))) %>% mutate(Het=value))
    GLH <- bind_rows(GLH,left_join(tidy(sum.temp),
                                   tidy(CI.temp)) %>% mutate(Het=value))
    
  } 
  #adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  # Clean names and labels
  GLH <- GLH %>% separate(contrast,into=c("Treat","term","Group"),sep=":") %>% 
    mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>%
    mutate(term=str_remove(term,"SubSample"),
           Group=str_remove(Group,"Het"))
  
  # join with MO
  GLH <- left_join(GLH,M0 %>% select(-c(estimate,std.error)))
  
  #Prepare Gof for the model
  
  Gof <- get_gof(model)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof <- Gof %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  #### Same thing for the control outcomes :
  
  
  # Tidy Database of the model
  
  M0.ctrl <- modelplot(model0,draw=FALSE) %>% 
    separate(term,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het")) %>% 
    rename("point.conf.low"="conf.low","point.conf.high"="conf.high")
  
  
  ## Joint hypothesis testing for each value of the heterogeneity variable
  GLH0 <-c()
  temp <- c()
  
  for (value in HetVal){
    temp <- glht(model0,
                 paste(paste("`",
                             names(model0$coefficients)[
                               str_detect(names(model0$coefficients),{{value}})],
                             "`= 0",sep="")))
    
    sum.temp <- summary(temp,adjusted(type={{Correction}}))
    CI.temp <- confint(temp)
    # GLH <- bind_rows(GLH,left_join(tidy(temp),
    #                               tidy(confint(temp))) %>% mutate(Het=value))
    GLH0 <- bind_rows(GLH0,left_join(tidy(sum.temp),
                                     tidy(CI.temp)) %>% mutate(Het=value))
    
  } 
  
  # Clean names and labels
  GLH0 <- GLH0 %>% separate(contrast,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het"))
  
  # join with MO
  GLH0 <- left_join(GLH0,M0.ctrl %>% select(-c(estimate,std.error)))
  
  #Prepare Gof for the model
  
  Gof0 <- get_gof(model0)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof0 <- Gof0 %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  
  
  
  
  
  ForModelSummary0 <- list(tidy=GLH0,
                           glance=Gof0
  )
  
  class(ForModelSummary0) <- "modelsummary_list"   # define the class
  
  
  #Prepare Gof for the model
  
  Gof <- get_gof(model)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof <- Gof %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  #%>% bind_cols(.,as.data.frame(t(Fstats)))
  
  
  ForModelSummary <- list(tidy=GLH,
                          glance=Gof
  )
  
  class(ForModelSummary) <- "modelsummary_list"   # define the class
  
  
  
  
  return(list("Estimation"=model,"Tidy"=GLH,"ModelSummary"=ForModelSummary,"Model 0"=model0,"ModelSummary0"=ForModelSummary0))    
}

#######################################################################################-
#######################################################################################-

#### CompareCoef ####

#######################################################################################-
#######################################################################################-

# Following a model using the previous function such as
#Het.ATT.UseCreche <- GroupHeterogeneityFnCTRL(ITT=FALSE)

# The following function allows to test the equality of heterogeneous parameters
# it returns a list ready for model summary, and a full table with all the results

CompareCoef <- function(Model = Het.ATT.UseCreche,
                        Padjust.Method = "Westfall",
                        OutcomeLabel="daycare use",
                        
                        GroupLabel="baseline education"
){
  
  
  # Testing equal coefficients
  MyList <- {{Model}}
  m = MyList[["Estimation"]]
  
  #names(m$coefficients)
  # 4 coefficients. 
  #Let's test T2-C:HetBac -T2-C:HetSup =0 and T2-T1:HetBac -T2-T1:HetSup =0 jointly
  
  # get the names of the variables in labelnames and clean a bit for the table after
  labelnames <- names(m$coefficients) %>% str_remove_all(.,"fit_|Z.c|Z|D|:SubSample")
  
  # First row tests first coef - third coef = 0
  # Second row tests second coef - fourth coef = 0
  K <- rbind(c(1, 0, -1,0),
             c(0, 1, 0,-1))
  
  #Let's put some names to the K matrix's row. 
  rownames(K) <- c(paste(labelnames[which(K[1,]==1)],"-",labelnames[which(K[1,]==-1)],"=0"),
                   paste(labelnames[which(K[2,]==1)],"-",labelnames[which(K[2,]==-1)],"=0"))
  # get the names of the coefficients for the columns
  colnames(K) <- names(coef(m))
  
  # See what this K matrix look like now that it has some nice names on it
  #K
  
  # Basic summary of testing jointly both rows of equality of coefficients (so difference = 0 against difference ≠0)
  glht.effect <- glht(m,K) 
  
  # How to get all these nice results in a modelsummary table:
  
  # First, tidy the glht result and get the confidence interval too 
  tidy.glht <- glht.effect %>% tidy(.,test=adjusted({{Padjust.Method}})) %>% left_join(.,
                                                                                       confint(glht.effect,adjusted={{Padjust.Method}}) %>% tidy()) %>% 
    rename(term=contrast) %>% 
    left_join(.,tidy(m)) %>% 
    mutate(period=str_remove(term, ":.*"),
           Group=str_remove_all(term,"T2-C:|T2-T1:"),
           Group=str_replace_all(Group,"Het","TE "),
           term=period,
           model="GLHT"
    )
  
  # second, get the statistics of the initial model in gof
  gof <- get_gof(m) %>% mutate("P adjust"={{Padjust.Method}})
  
  #put them in a list
  
  list.Glht <- list(tidy=tidy.glht,
                    glance=gof
  )
  
  # tell R it's a modelsummy type of list
  
  class(list.Glht) <- "modelsummary_list"   # define the class
  
  
  TheModels <-   list(
    MyList$ModelSummary0,
    MyList$ModelSummary,
    list.Glht)
  
  TheTitle = paste("Average treatment effect by",{{GroupLabel}}, "on", {{OutcomeLabel}})
  
  cm <- c('T2-C'    = 'Support + Information vs Control',
          'T2-T1' = 'Support vs Information')
  
  
  names(TheModels) <- c(paste({{OutcomeLabel}},"Avg. cfct.",sep="_"),
                        paste({{OutcomeLabel}},"CATE",sep="_"),
                        paste({{OutcomeLabel}},"Test inference",sep="_"))
  
  Summary.Model <- modelsummary(TheModels,
                                shape = term + Group ~ model,
                                coef_map = cm,
                                fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = c("conf.int",
                                              "adj.p.val. = {adj.p.value}"),
                                stars = c('*' = .1,'**' = .05, '***' = .01),
                                gof_map = c(#"Mean of DV",
                                  "Covariates","Fixed effects","Mean F-stat 1st stage","P adjust",
                                  "nobs", "r.squared","adj.r.squared"),
                                title=TheTitle,
                                notes=paste("Sources:", SourcesStacked,
                                            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference." 
                                ),output = 'flextable') %>% 
    theme_booktabs()|>
    separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
    bold(i=1,  part = "header") %>%                # Variable labels bold
    merge_at(j=2,part="header")|>
    merge_at(j=1,part="header")|>
    merge_v(j=1,part="body")|>
    italic(i = c(1),  part = "header") %>% 
    italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
    align(part = "header", align = "center")|>                # center
    align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
    width(j=c(3,4,5),width=2.7,unit = "cm")|>
    width(j=c(1,2),width=2.4,unit = "cm") %>% 
    hline(c(9,18),part="body")
  
  
  return(list("CompareCoef" = list.Glht,"Summary.Model"=Summary.Model))
  
}


# 


#######################################################################################-
#######################################################################################-

#### CompareCoefDelta ####

#######################################################################################-
#######################################################################################-

# Following a model using the previous function such as
#Het.ATT.UseCreche <- GroupHeterogeneityFnCTRL(ITT=FALSE,Heterogeneity = "MigrationBackground")

# The following function allows to test the equality of heterogeneous gaps in parameters
# it returns two lists ready for model summary, and a full table with all the results


CompareCoefDelta <- function(Model = Het.ATT.UseCreche,
                             Padjust.Method = "Westfall",
                             OutcomeLabel="daycare use",
                             
                             GroupLabel="baseline education"
){
  
  
  # Testing equal coefficients
  MyList <- {{Model}}
  m = MyList[["Estimation"]]
  m0 = MyList[["Model 0"]]
  
  
  # get the names of the variables in labelnames and clean a bit for the table after
  labelnames <- names(m$coefficients) %>% str_remove_all(.,"fit_|Z.c|Z|D|:SubSample")
  
  
  # First row tests first coef - third coef = 0
  # Second row tests second coef - fourth coef = 0
  K <- rbind(c(1, 0, -1,0),
             c(0, 1, 0,-1))
  
  #Let's put some names to the K matrix's row. 
  rownames(K) <- c(paste(labelnames[which(K[1,]==1)],"-",labelnames[which(K[1,]==-1)],"=0"),
                   paste(labelnames[which(K[2,]==1)],"-",labelnames[which(K[2,]==-1)],"=0"))
  # get the names of the coefficients for the columns
  colnames(K) <- names(coef(m))
  
  # See what this K matrix look like now that it has some nice names on it
  #K
  
  # Basic summary of testing jointly both rows of equality of coefficients (so difference = 0 against difference ≠0)
  glht.effect <- glht(m,K) 
  
  # How to get all these nice results in a modelsummary table:
  
  # First, tidy the glht result and get the confidence interval too 
  tidy.glht <- glht.effect %>% tidy(test=adjusted({{Padjust.Method}})) %>% left_join(.,
                                                                                     tidy(confint(glht.effect,test=adjusted({{Padjust.Method}}))) ) %>% 
    rename(term=contrast) %>% 
    left_join(.,tidy(m)) %>% 
    mutate(period=str_remove(term, ":.*"),
           Group=str_remove_all(term,"T2-C:|T2-T1:"),
           Group=str_replace_all(Group,"Het","TE "),
           term=period,
           term2="Conditional treatment effect"
    )
  
  
  # second, get the statistics of the initial model in gof
  gof <- get_gof(m) %>% mutate("P adjust"={{Padjust.Method}})
  
  #put them in a list
  
  list.Glht <- list(tidy=tidy.glht,
                    glance=gof
  )
  
  # tell R it's a modelsummy type of list
  
  class(list.Glht) <- "modelsummary_list"   # define the class
  
  
  
  
  ###### Difference in counterfactual
  # Basic summary of testing jointly both rows of equality of coefficients (so difference = 0 against difference ≠0)
  glht.cf <- glht(m0,K) 
  
  # How to get all these nice results in a modelsummary table:
  
  # First, tidy the glht result and get the confidence interval too 
  tidy.glht.cf <- glht.cf %>% tidy(test=adjusted({{Padjust.Method}})) %>% left_join(.,
                                                                                    tidy(confint(glht.cf,test=adjusted({{Padjust.Method}}))) ) %>% 
    rename(term=contrast) %>% 
    left_join(.,tidy(m0)) %>% 
    mutate(period=str_remove(term, ":.*"),
           Group=str_remove_all(term,"T2-C:|T2-T1:"),
           Group=str_replace_all(Group,"Het","TE "),
           term=period,
           term2="Conditional gap"
    )
  
  
  
  # second, get the statistics of the initial model in gof
  gof.m0 <- get_gof(m0) %>% mutate("P adjust"={{Padjust.Method}})
  
  #put them in a list
  
  list.Glht0 <- list(tidy=tidy.glht.cf,
                     glance=gof.m0
  )
  
  # tell R it's a modelsummy type of list
  
  class(list.Glht0) <- "modelsummary_list"   # define the class
  
  
  
  
  
  
  TheModels <-   list(
    MyList$ModelSummary0,
    MyList$ModelSummary,
    list.Glht0,
    list.Glht)
  
  TheTitle = paste("Average treatment effect by",{{GroupLabel}}, "on", {{OutcomeLabel}})
  
  isIV <- sum(names(m)=='iv')>0
  
  if (isIV){
    
    names(TheModels) <- c(paste({{OutcomeLabel}},"Avg. cfct.",sep="_"),
                          paste({{OutcomeLabel}},"Conditional LATE",sep="_"),
                          paste({{OutcomeLabel}},"Average gap in cft.",sep="_"),
                          paste({{OutcomeLabel}},"∆ Conditional LATE",sep="_"))
  }
  else{
    
    names(TheModels) <- c(paste({{OutcomeLabel}},"Avg. ctrl.",sep="_"),
                          paste({{OutcomeLabel}},"Conditional ITT",sep="_"),
                          paste({{OutcomeLabel}},"∆ ctrl.",sep="_"),
                          paste({{OutcomeLabel}},"∆ ITT",sep="_"))
  }
  
  
  cm <- c('T2-C'    = 'Information + Support vs Control',
          'T2-T1' = 'Support vs Information')
  
  Summary.Model <- modelsummary(TheModels,
                                shape =  Group ~ model,
                                coef_map = cm,
                                fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = c("conf.int",
                                              "adj.p.val. = {adj.p.value}"),
                                stars = c('*' = .1,'**' = .05, '***' = .01),
                                gof_map = c(#"Mean of DV",
                                  "Covariates","Fixed effects","Mean F-stat 1st stage","P adjust",
                                  "nobs", "r.squared","adj.r.squared"),
                                title=TheTitle,
                                notes=paste("Sources:", SourcesStacked,
                                            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference." 
                                ),output = 'flextable') %>% 
    theme_booktabs()|>
    separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
    bold(i=1,  part = "header") %>%                # Variable labels bold
    merge_at(j=2,part="header")|>
    merge_at(j=1,part="header")|>
    merge_v(j=1,part="body")|>
    italic(i = c(1),  part = "header") %>% 
    italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
    align(part = "header", align = "center")|>                # center
    align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
    width(j=c(3,4,5),width=2.7,unit = "cm")|>
    width(j=c(1,2),width=2.4,unit = "cm") %>% 
    hline(c(9,18),part="body")
  
  Summary.Model <- modelsummary(TheModels,
                                shape =  Group ~ model,
                                coef_map = cm,
                                fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = c("conf.int",
                                              "adj.p.val. = {adj.p.value}"),
                                stars = c('*' = .1,'**' = .05, '***' = .01),
                                gof_map = c(#"Mean of DV",
                                  "Covariates","Fixed effects","Mean F-stat 1st stage","P adjust",
                                  "nobs", "r.squared","adj.r.squared"),
                                title=TheTitle,
                                notes=paste("Sources:", SourcesStacked,
                                            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference." 
                                ),output = 'flextable') %>% 
    theme_booktabs()|>
    separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
    bold(i=1,  part = "header") %>%                # Variable labels bold
    merge_at(j=2,part="header")|>
    merge_at(j=1,part="header")|>
    merge_v(j=1,part="body")|>
    italic(i = c(1),  part = "header") %>% 
    italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
    align(part = "header", align = "center")|>                # center
    align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
    width(j=c(3,4,5),width=2.7,unit = "cm")|>
    width(j=c(1,2),width=2.4,unit = "cm") %>% 
    hline(c(9,18),part="body")
  
  
  return(list("CompareCoef" = list.Glht,"Compare.Y0"=list.Glht0,   "Summary.Model"=Summary.Model))
  
}

# test <- CompareCoefDelta(Het.ATT.UseCreche)









# test$Summary.Model
#              
#              ,
#              shape = term + Group ~ model,
#              fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
#              
#              )
#{Model}[["Estimation"]]
#tst <- GroupHeterogeneityFnCTRL(ITT=FALSE)



#########################################################################################

###########################################################################################

ITTSimNoControl <- function(Y="UseCreche",
                            treat="Z",
                            DB=PostDB,
                            Correction="Westfall",
                            weights="WeightPS"
){
  #Correction methods available
  # c('single-step', 'Shaffer', 'Westfall', 'free', 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none')
  
  
  #Data
  DBInside <- DB 
  # outcome
  DBInside$Y <- DBInside[[{{Y}}]]  
  # treatment variable
  DBInside$Z <- DBInside[[{{treat}}]]
  
  # weights : initialized as 1 if no weights given
  if (weights==""){
    DBInside$w=1  
  }else{
    DBInside$w <- DBInside[[{{weights}}]]  
  }
  
  #model  
  model <- feols(Y~i(Z,SubSample,ref=0)|SubSampleStrata,DBInside,cluster = ~StrataWave,weights = ~w)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("Z","term","Var"),sep="::") %>% mutate(term=Var,
                                                           Var=str_remove_all(Var,"MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )
  
  
  ### prepare for model summary
  
  ChisQTest <- glht(model) %>% summary(.,test=Chisqtest())
  
  fullModel <- list(tidy=tidy.final, # list with tidy containing the dataframe with the estimates, # list with tidy containing the dataframe with the estimates
                    glance=get_gof(model) %>%  # statistics of the model
                      bind_cols(.,"Fixed effects"="X") %>% 
                      bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                      "P-value"=  ChisQTest$test$pvalue)
                      )))
  class(fullModel) <- "modelsummary_list"   # define the class
  
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}},"ModelSummary"=fullModel))    
}


#



# Statistical Reporting Functions for R Markdown

# Function to extract and format a single statistical result
format_stat_result <- function(tidy_df, term_name, df_value = NULL, digits = 2) {
  # Extract the row for the specified term
  row_data <- tidy_df[tidy_df$term == term_name, ]
  
  if (nrow(row_data) == 0) {
    warning(paste("Term", term_name, "not found in results"))
    return("Term not found")
  }
  
  # Extract values with proper rounding
  estimate <- round(row_data$estimate, digits)
  std_error <- round(row_data$std.error, digits)
  conf_low <- round(row_data$conf.low, digits)
  conf_high <- round(row_data$conf.high, digits)
  p_value <- round(row_data$p.value, digits + 1)  # One more digit for p-values
  adj_p_value <- round(row_data$adj.p.value, digits + 1)
  
  # Format p-values properly (show < 0.001 for very small values)
  #p_formatted <- ifelse(p_value < 0.001, "< 0.001", as.character(p_value))
  #adj_p_formatted <- ifelse(adj_p_value < 0.001, "< 0.001", as.character(adj_p_value))
  p_formatted <- p_value
  adj_p_formatted <- adj_p_value
  # Create the formatted string
  if (is.null(df_value)) {
    result <- paste0("$\\beta$ = ", estimate, 
                     ", SE = ", std_error, 
                     ", 95% SCI = (", conf_low, "; ", conf_high, ")", 
                     ", p = ", p_formatted, 
                     ", adj.p = ", adj_p_formatted)
  } else {
    result <- paste0("$\\beta$(", df_value, ") = ", estimate, 
                     ", SE = ", std_error, 
                     ", 95% SCI = (", conf_low, "; ", conf_high, ")", 
                     ", p = ", p_formatted, 
                     ", adj.p = ", adj_p_formatted)
  }
  
  return(result)
}

# Function to create a comprehensive results object from multiple analyses
create_results_object <- function(analysis_list, df_values = NULL) {
  results <- list()
  
  for (i in seq_along(analysis_list)) {
    analysis_name <- names(analysis_list)[i]
    tidy_df <- analysis_list[[i]]$Tidy
    
    # Get DF value if provided
    df_val <- if (!is.null(df_values)) df_values[[analysis_name]] else NULL
    
    # Extract all terms from this analysis
    terms <- unique(tidy_df$term)
    
    for (term in terms) {
      # Create a clean name for the result
      result_name <- paste(analysis_name, term, sep = ".")
      result_name <- gsub("-", "_", result_name)  # Replace hyphens with underscores
      
      # Store the formatted result
      results[[result_name]] <- format_stat_result(tidy_df, term, df_val)
    }
  }
  
  return(results)
}

text_lasso <- function(row_data=data_plot_lasso,SubSample="T2-C",Y="Early childcare application",
                       Method="Post double lasso",
                       df_value=83,
                       digits=2){
  row_data <- row_data %>% filter(SubSample=={{SubSample}} & Y=={{Y}} & Method=={{Method}})
  # Extract values with proper rounding
  estimate <- round(row_data$estimate, digits)
  std_error <- round(row_data$std.error, digits)
  conf_low <- round(row_data$conf.low, digits)
  conf_high <- round(row_data$conf.high, digits)
  p_value <- round(row_data$p.value, digits + 1)  # One more digit for p-values
  p_formatted <- p_value
  
  
  # Create the formatted string
  if (is.null(df_value)) {
    result <- paste0("$\\beta_{lasso}$ = ", estimate, 
                     ", SE = ", std_error, 
                     ", 95% SCI = (", conf_low, "; ", conf_high, ")", 
                     ", p = ", p_formatted)
  }else{
  result <- paste0("$\\beta_{lasso}$(", df_value, ") = ", estimate, 
                   ", SE = ", std_error, 
                   ", 95% SCI = (", conf_low, "; ", conf_high, ")", 
                   ", p = ", p_formatted)
  
  }
  return(result)
}

text_het <- function(row_data=Data.Het.EducMig,SubSample="T2-C",
                     Y="Apply for early childcare",
                     Group="Low-SES",
                     panel="ITT",
                       df_value=83,
                       digits=2){
  row_data <- row_data %>% filter(term=={{SubSample}} & Y=={{Y}} & Group=={{Group}} & panel=={{panel}})
  # Extract values with proper rounding
  estimate <- round(row_data$estimate, digits)
  std_error <- round(row_data$std.error, digits)
  conf_low <- round(row_data$conf.low, digits)
  conf_high <- round(row_data$conf.high, digits)
  p_value <- round(row_data$p.value, digits + 1)  # One more digit for p-values
  adj_p_value <- round(row_data$adj.p.value, digits + 1)
  p_formatted <- p_value
  adj_p_formatted <- adj_p_value
  
  # Create the formatted string
  if (is.null(df_value)) {
    result <- paste0("$\\beta_{",{{Group}},"}$ = ", estimate, 
                     ", SE = ", std_error, 
                     ", 95% SCI = (", conf_low, "; ", conf_high, ")", 
                     ", p = ", p_formatted,
                     ", adj.p = ", adj_p_formatted)
  }else{
    result <- paste0("$\\beta_{",{{Group}},"}$(", df_value, ") = ", estimate, 
                     ", SE = ", std_error, 
                     ", 95% SCI = (", conf_low, "; ", conf_high, ")", 
                     ", p = ", p_formatted,
                     ", adj.p = ", adj_p_formatted)
    
  }
  return(result)
}





#######################################################################################-
#######################################################################################-

#### Graphs Preferred Mode  ####

#######################################################################################-
#######################################################################################-

library(stringr)


# Function to count reasons by group
count_reasons_by_group <- function(data_column, group_column) {
  # Identify the unique levels
  group_levels <- unique(group_column[!is.na(group_column)])
  
  # Filter NAs
  valid_data <- data.frame(
    response = data_column,
    group = group_column
  ) %>% filter(!is.na(response))
  
  # Count the reasons
  count_reason <- function(data, patterns, group_value) {
    mentions <- unlist(strsplit(data$response[data$group == group_value], ","))
    if(length(mentions) == 0) return(0)
    sum(sapply(mentions, function(x) any(sapply(patterns, function(p) str_detect(x, fixed(p))))))
  }
  
  # List of patterns
  reasons_patterns <- list(
    "Too expensive" = c("Le mode de garde que je préférais coûtait trop cher", "Too expensive"),
    "No slot available" = c("Je n'ai pas eu de place dans le mode de garde que je préférais", "Rejected", "Still waiting/No Answer"),
    "Incompatible working hours" = c("Mes horaires de travail ne sont pas compatibles", "Incompatible working hours"),
    "Health issues" = c("Mon bébé n'allait pas bien / les médecins m'ont déconseillés", "Health issues"),
    "Lack of information" = c("Je pensais que je n'étais pas éligible", "Lack of information"),
    "Gave up" = "Je me suis découragée",
    "Applied too late" = "Applied too late",
    "Inadequate" = "Inadequate",
    "Father's disagreed" = "Father's disagreed",
    "Moved out" = "Moved out",
    "Paperwork too heavy" = "Paperwork too heavy",
    "Timing" = "Timing",
    "Too far away" = "Too far away"
  )
  
  # Count of reasons per group
  counts_list <- lapply(group_levels, function(level) {
    sapply(reasons_patterns, function(patterns) count_reason(valid_data, patterns, level))
  })
  
  # Create dataframe with results
  result_df <- data.frame(
    reason = rep(names(reasons_patterns), length(group_levels)),
    count = unlist(counts_list),
    group = rep(group_levels, each = length(reasons_patterns))
  )
  
  # Calculate significance using chi-square test for each reason
  significance_tests <- lapply(unique(result_df$reason), function(r) {
    contingency <- matrix(
      result_df$count[result_df$reason == r],
      nrow = 2,
      byrow = FALSE
    )
    test <- chisq.test(contingency)
    data.frame(
      reason = r,
      p_value = test$p.value,
      significance = case_when(
        test$p.value < 0.001 ~ "***",
        test$p.value < 0.01 ~ "**",
        test$p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  })
  
  significance_df <- do.call(rbind, significance_tests)
  result_df <- merge(result_df, significance_df, by = "reason")
  
  # Calculate totals and order
  totals <- aggregate(count ~ reason, result_df, sum)
  result_df$reason <- factor(result_df$reason, 
                             levels = totals$reason[order(totals$count)])
  
  return(result_df)
}

# Create plot function with significance stars
create_reasons_plot <- function(results_df, title = "Reasons by Group") {
  ggplot(results_df, aes(x = reason, y = count, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#EE6677", "#CCBB44")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = title,
      x = "Reason",
      y = "Count"
    ) +
    coord_flip() 
  # Add significance stars
  # +geom_text(aes(label = significance), 
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.5, 
  #           size = 4)
}
