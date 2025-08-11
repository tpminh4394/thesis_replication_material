library(dplyr)
library(lavaan)
library(purrr)
library(fixest)
library(texreg)
library(ggplot2)
library(broom)
library(tibble)
library(writexl)
library(ggrepel)


################# Specify the models ####################

model_cs <- '
conservation =~ impsafe + ipstrgv + ipfrule + ipbhprp + imptrad + ipmodst
ipfrule ~~ ipbhprp             # Conformity
imptrad ~~ ipmodst             # Tradition
impsafe ~~ ipstrgv            # Security
'

model_st <- '
# Self-Transcendence (directly including the observed items)
self_transcendence =~  iphlppl + iplylfr + ipeqopt + ipudrst + impenv
iphlppl ~~ iplylfr             # Benevolence
ipeqopt ~~ ipudrst          # Universalism

'

model_oc <- '
# Openness to Change (directly including the observed items)
openness_to_change =~ impdiff + ipadvnt + ipcrtiv + impfree
impdiff ~~ ipadvnt            #Stimulation
'


model_se <- '
# Self-Enhancement (directly including the observed items)
self_enhancement =~ ipshabt + ipsuces + imprich + iprspot +ipgdtim + impfun
imprich ~~  iprspot             # Power
ipshabt ~~ ipsuces             # Achievement
ipgdtim ~~ impfun             #Hedonism
'


################# Reverse the item question and calculate response type ####################

combined_data_com_country_filtered <-combined_data_com_country_filtered_org %>% mutate(across(c(
  ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ipshabt, 
  ipsuces, imprich, iprspot, impsafe, ipstrgv, ipfrule, ipbhprp, 
  imptrad, ipmodst, iphlppl, iplylfr, ipeqopt, ipudrst, impenv 
),~ 7-.))

all_items <- c("ipcrtiv", "impfree", "impdiff", "ipadvnt", "ipgdtim", "impfun",
               "ipsuces", "imprich", "iprspot", "ipshabt",
               "impsafe", "ipstrgv", "ipfrule", "ipbhprp", "imptrad", "ipmodst",
               "iphlppl", "iplylfr", "ipeqopt", "ipudrst", "impenv")


combined_data_com_country_filtered <- combined_data_com_country_filtered %>%
  mutate(
    mean_restype_oc = rowMeans(select(., all_of(setdiff(all_items, 
                                                        c("ipcrtiv", "impfree", "impdiff", "ipadvnt" )))), na.rm = TRUE),
    
    mean_restype_cs = rowMeans(select(., all_of(setdiff(all_items, 
                                                        c("impsafe", "ipstrgv", "ipfrule", "ipbhprp", "imptrad", "ipmodst")))), na.rm = TRUE),
    
    mean_restype_st = rowMeans(select(., all_of(setdiff(all_items, 
                                                        c("iphlppl", "iplylfr", "ipeqopt", "ipudrst", "impenv")))), na.rm = TRUE),
    
    mean_restype_se = rowMeans(select(., all_of(setdiff(all_items, 
                                                        c("ipsuces", "imprich", "iprspot", "ipshabt", "ipgdtim", "impfun")))), na.rm = TRUE)
  )



################# Function checking measurement variance ####################


check_partial_scalar_invariance <- function(model, df_cty, cty, max_free = 3) {
  library(lavaan)
  library(dplyr)
  library(tibble)
  
  # Helper function to fit model and extract fit indices
  fit_model <- function(group.equal = NULL, group.partial = NULL) {
    fit <- tryCatch({
      cfa(model, data = df_cty, group = "essround", std.lv = TRUE,
          group.equal = group.equal, group.partial = group.partial, 
          missing = "fiml")
    }, error = function(e) {
      message(paste("Model fitting error:", e$message))
      return(NULL)
    })
    
    if (is.null(fit)) return(NULL)
    
    # Extract fit measures safely and convert to numeric
    fit_measures <- tryCatch({
      list(
        fit = fit,
        cfi = as.numeric(fitMeasures(fit, "cfi")),
        rmsea = as.numeric(fitMeasures(fit, "rmsea"))
      )
    }, error = function(e) {
      message(paste("Fit measures extraction error:", e$message))
      return(NULL)
    })
    
    return(fit_measures)
  }
  
  # Helper function to extract loadings from a fitted model
  extract_loadings <- function(fit, model_type, country) {
    if (is.null(fit)) return(NULL)
    
    tryCatch({
      params <- standardizedSolution(fit) %>%
        filter(op == "=~") %>%
        select(lhs, rhs, group, est.std, se, z, pvalue) %>%
        mutate(
          country = country,
          model_type = model_type,
          factor = lhs,
          item = rhs,
          loading = round(est.std, 3),
          se = round(se, 3),
          z_value = round(z, 3),
          p_value = round(pvalue, 4)
        ) %>%
        select(country, model_type, group, factor, item, loading, se, z_value, p_value)
      
      return(params)
    }, error = function(e) {
      message(paste("Error extracting loadings:", e$message))
      return(NULL)
    })
  }
  
  # Initialize output structure
  out <- list(
    country = cty,
    configural_status = NA,
    metric_status = NA,
    scalar_status = NA,
    cfi_config = NA, rmsea_config = NA,
    cfi_metric = NA, rmsea_metric = NA,
    cfi_scalar = NA, rmsea_scalar = NA,
    delta_cfi_metric = NA, delta_rmsea_metric = NA,
    delta_cfi_scalar = NA, delta_rmsea_scalar = NA,
    freed_loadings = NA, freed_intercepts = NA
  )
  # Initialize loadings storage - will only be filled if scalar invariance is achieved
  final_loadings <- NULL
  final_model_fit <- NULL  # Store the final model that achieved scalar invariance
  
  
  # STEP 1: CONFIGURAL INVARIANCE
  message(paste("Testing configural invariance for", cty))
  
  cfg <- fit_model()
  if (is.null(cfg)) {
    out$configural_status <- "error"
    return(as_tibble(out))
  }
  
  out$cfi_config <- as.numeric(cfg$cfi)
  out$rmsea_config <- as.numeric(cfg$rmsea)
  
  # Check configural criteria (CFI >= 0.95 AND RMSEA <= 0.06)
  configural_satisfied <- (round(cfg$cfi, 2) >= 0.90 && round(cfg$rmsea, 2) <= 0.08)
  out$configural_status <- ifelse(configural_satisfied, "satisfied", "not satisfied")
  
  message(paste("Configural:", out$configural_status, 
                "(CFI =", round(cfg$cfi, 3), ", RMSEA =", round(cfg$rmsea, 3), ")"))
  
  if (!configural_satisfied) {
    return(list(country = cty,results = as_tibble(out), loadings = NULL, model = NULL ))
    
  }
  
  # STEP 2: METRIC INVARIANCE
  message(paste("Testing metric invariance for", cty))
  
  met_full <- fit_model(group.equal = "loadings")
  if (is.null(met_full)) {
    out$metric_status <- "error"
    return(list(country = cty,results = as_tibble(out), loadings = NULL, model = NULL ))
  }
  
  # Calculate change in fit indices
  delta_cfi_metric_full <- as.numeric(met_full$cfi) - as.numeric(out$cfi_config)
  delta_rmsea_metric_full <- as.numeric(met_full$rmsea) - as.numeric(out$rmsea_config)
  
  # Check metric invariance criteria (ΔCFI >= -0.01 AND ΔRMSEA <= 0.015)
  metric_satisfied <- (round(delta_cfi_metric_full, 2) >= -0.01 && round(delta_rmsea_metric_full, 3) <= 0.015)
  
  if (metric_satisfied) {
    out$metric_status <- "full"
    out$cfi_metric <- as.numeric(met_full$cfi)
    out$rmsea_metric <- as.numeric(met_full$rmsea)
    out$delta_cfi_metric <- delta_cfi_metric_full
    out$delta_rmsea_metric <- delta_rmsea_metric_full
    
    message(paste("Metric: full invariance achieved",
                  "(ΔCFI =", round(delta_cfi_metric_full, 2), 
                  ", ΔRMSEA =", round(delta_rmsea_metric_full, 3), ")"))
  } else {
    # Test partial metric invariance
    message("Testing partial metric invariance...")
    
    # Get all factor loading specifications (factor=~item)
    loading_specs <- tryCatch({
      param_est <- parameterEstimates(met_full$fit) %>% 
        filter(op == "=~") %>% 
        select(lhs, rhs)
      paste0(param_est$lhs, "=~", param_est$rhs)
    }, error = function(e) {
      message("Error extracting loading specifications for partial metric invariance")
      return(character(0))
    })
    
    if (length(loading_specs) == 0) {
      out$metric_status <- "error"
      return(list(country = cty,results = as_tibble(out), loadings = NULL, model = NULL ))
    }
    
    candidates <- list()
    
    # Try freeing 1 to max_free parameters
    for (n_free in 1:min(max_free, length(loading_specs))) {
      message(paste("Trying to free", n_free, "loading(s)..."))
      
      for (combo in combn(loading_specs, n_free, simplify = FALSE)) {
        partial_fit <- fit_model(group.equal = "loadings", 
                                 group.partial = combo)
        
        if (!is.null(partial_fit)) {
          delta_cfi <- as.numeric(partial_fit$cfi) - as.numeric(out$cfi_config)
          delta_rmsea <- as.numeric(partial_fit$rmsea) - as.numeric(out$rmsea_config)
          
          if (round(delta_cfi, 2) >= -0.01 && round(delta_rmsea, 3) <= 0.015) {
            # Create a readable key for the combination
            combo_key <- paste(sapply(combo, function(x) {
              parts <- strsplit(x, "=~")[[1]]
              parts[2]  # just the item name for the key
            }), collapse = ",")
            
            candidates[[combo_key]] <- list(
              fit = partial_fit,
              delta_cfi = delta_cfi,
              delta_rmsea = delta_rmsea,
              freed_items = combo
            )
          }
        }
      }
      
      if (length(candidates) > 0) break
    }
    
    if (length(candidates) > 0) {
      # Select candidate with lowest RMSEA
      rmsea_values <- sapply(candidates, function(x) x$fit$rmsea)
      best_index <- which.min(rmsea_values)
      best_key <- names(candidates)[best_index]
      best_candidate <- candidates[[best_key]]
      
      out$metric_status <- paste0("partial (", best_key, ")")
      out$cfi_metric <- as.numeric(best_candidate$fit$cfi)
      out$rmsea_metric <- as.numeric(best_candidate$fit$rmsea)
      out$delta_cfi_metric <- best_candidate$delta_cfi
      out$delta_rmsea_metric <- best_candidate$delta_rmsea
      out$freed_loadings <- best_key
      
      message(paste("Partial metric invariance achieved by freeing:", best_key))
    } else {
      out$metric_status <- "none"
      out$cfi_metric <- as.numeric(met_full$cfi)
      out$rmsea_metric <- as.numeric(met_full$rmsea)
      out$delta_cfi_metric <- delta_cfi_metric_full
      out$delta_rmsea_metric <- delta_rmsea_metric_full
      
      message("No acceptable partial metric invariance found")
    }
  }
  
  # STEP 3: SCALAR INVARIANCE
  if (out$metric_status != "none" && !is.na(out$cfi_metric)) {
    message(paste("Testing scalar invariance for", cty))
    
    # Set up constraints for scalar model
    group_equal <- c("loadings", "intercepts")
    partial_loadings <- if (!is.na(out$freed_loadings)) {
      # Reconstruct the full loading specifications from the freed items
      freed_items <- trimws(unlist(strsplit(out$freed_loadings, ",")))
      param_est <- parameterEstimates(met_full$fit) %>% 
        filter(op == "=~" & rhs %in% freed_items) %>% 
        select(lhs, rhs)
      paste0(param_est$lhs, "=~", param_est$rhs)
    } else {
      NULL
    }
    
    # Test full scalar invariance (only if metric was full)
    if (out$metric_status == "full") {
      scl_full <- fit_model(group.equal = group_equal)
      
      if (!is.null(scl_full)) {
        delta_cfi_scalar_full <- as.numeric(scl_full$cfi) - as.numeric(out$cfi_metric)
        delta_rmsea_scalar_full <- as.numeric(scl_full$rmsea) - as.numeric(out$rmsea_metric)
        
        if (round(delta_cfi_scalar_full, 2) >= -0.01 && round(delta_rmsea_scalar_full, 3) <= 0.015) {
          out$scalar_status <- "full"
          out$cfi_scalar <- as.numeric(scl_full$cfi)
          out$rmsea_scalar <- as.numeric(scl_full$rmsea)
          out$delta_cfi_scalar <- delta_cfi_scalar_full
          out$delta_rmsea_scalar <- delta_rmsea_scalar_full
          
          message("Full scalar invariance achieved")
          final_model_fit <- scl_full$fit
          final_loadings <- extract_loadings(final_model_fit, "scalar_full", cty)
          
          #          return(as_tibble(out))
          return(list(country = cty,results = as_tibble(out), loadings = final_loadings %||% tibble(),model =  scl_full$fit  ))
          
        }
      }
    }
    
    # Test partial scalar invariance
    message("Testing partial scalar invariance...")
    
    # Get intercept parameters
    intercepts <- tryCatch({
      if (out$metric_status == "full") {
        unique(parameterEstimates(met_full$fit) %>% 
                 filter(op == "~1") %>% 
                 pull(lhs))
      } else {
        # Use the partial metric model to get intercepts
        freed_items <- trimws(unlist(strsplit(out$freed_loadings, ",")))
        param_est <- parameterEstimates(met_full$fit) %>% 
          filter(op == "=~" & rhs %in% freed_items) %>% 
          select(lhs, rhs)
        partial_load_constraints <- paste0(param_est$lhs, "=~", param_est$rhs)
        partial_met <- fit_model(group.equal = "loadings", group.partial = partial_load_constraints)
        if (!is.null(partial_met)) {
          unique(parameterEstimates(partial_met$fit) %>% 
                   filter(op == "~1") %>% 
                   pull(lhs))
        } else {
          character(0)
        }
      }
    }, error = function(e) {
      message("Error extracting intercepts for partial scalar invariance")
      return(character(0))
    })
    
    if (length(intercepts) == 0) {
      out$scalar_status <- "error"
      #      return(as_tibble(out))
      return(list(country = cty,results = as_tibble(out), loadings = NULL, model = NULL  ))
      
    }
    
    candidates <- list()
    
    
    # Filter intercepts to separate forced vs optional
    freed_loading_items <- if (!is.na(out$freed_loadings)) {
      freed_items  # This was already created above
    } else {
      character(0)
    }
    
    forced_intercepts <- intersect(intercepts, freed_loading_items)
    remaining_intercepts <- setdiff(intercepts, forced_intercepts)
    
    for (n_free in 1:min(max_free, length(intercepts))) {
      message(paste("Trying to free", n_free, "intercept(s)..."))
      
      # Calculate how many additional intercepts we can choose
      n_additional <- n_free - length(forced_intercepts)
      
      if (n_additional < 0) {
        # Skip if we're trying to free fewer than the forced intercepts
        next
      } else if (n_additional == 0) {
        # Only use forced intercepts
        combos_list <- list(forced_intercepts)
      } else if (length(remaining_intercepts) >= n_additional) {
        # Generate combinations of remaining intercepts
        additional_combos <- combn(remaining_intercepts, n_additional, simplify = FALSE)
        # Add forced intercepts to each combination
        combos_list <- lapply(additional_combos, function(x) c(forced_intercepts, x))
      } else {
        # Not enough remaining intercepts
        next
      }
      
      for (combo in combos_list) {
        partial_intercepts <- paste0(combo, "~1")
        all_partial <- c(partial_loadings, partial_intercepts)
        
        
        
        partial_fit <- fit_model(group.equal = group_equal, 
                                 group.partial = all_partial)
        
        if (!is.null(partial_fit)) {
          delta_cfi <- as.numeric(partial_fit$cfi) - as.numeric(out$cfi_metric)
          delta_rmsea <- as.numeric(partial_fit$rmsea) - as.numeric(out$rmsea_metric)
          
          if (round(delta_cfi, 2) >= -0.01 && round(delta_rmsea, 3) <= 0.015) {
            candidates[[paste(combo, collapse = ",")]] <- list(
              fit = partial_fit,
              delta_cfi = delta_cfi,
              delta_rmsea = delta_rmsea,
              freed_intercepts = combo
            )
          }
        }
      }
      
      if (length(candidates) > 0) break
    }
    
    if (length(candidates) > 0) {
      # Select candidate with lowest RMSEA
      rmsea_values <- sapply(candidates, function(x) x$fit$rmsea)
      best_index <- which.min(rmsea_values)
      best_key <- names(candidates)[best_index]
      best_candidate <- candidates[[best_key]]
      
      out$scalar_status <- paste0("partial (", best_key, ")")
      out$cfi_scalar <- as.numeric(best_candidate$fit$cfi)
      out$rmsea_scalar <- as.numeric(best_candidate$fit$rmsea)
      out$delta_cfi_scalar <- best_candidate$delta_cfi
      out$delta_rmsea_scalar <- best_candidate$delta_rmsea
      out$freed_intercepts <- best_key
      
      # STORE LOADINGS - Partial scalar invariance achieved
      final_model_fit <- best_candidate$fit$fit
      final_loadings <- extract_loadings(final_model_fit, "scalar_partial", cty)
      
      message(paste("candidate list:", names(candidates)))
      message(paste("Partial scalar invariance achieved by freeing:", best_key))
    } else {
      out$scalar_status <- "none"
      out$cfi_scalar <- NA
      out$rmsea_scalar <- NA
      out$delta_cfi_scalar <- NA
      out$delta_rmsea_scalar <- NA
      
      message("No acceptable partial scalar invariance found")
    }
  } else {
    out$scalar_status <- "not tested"
    message("Scalar invariance not tested due to inadequate metric invariance")
  }
  
  return(list(country = cty,results = as_tibble(out), loadings = final_loadings %||% tibble(), model = tryCatch(
    best_candidate$fit$fit,
    error = function(e) NULL
  )))
  
}

#########################running verification function and checking result#################################


countries <- unique(combined_data_com_country_filtered$cntry)
######OC#########
results_list_conso_oc <- purrr::map( countries , function(cty) {
  df_cty <- combined_data_com_country_filtered %>%
    filter(cntry == cty, essround %in% 9:11) %>%
    mutate(essround = as.factor(essround))
  check_partial_scalar_invariance(model_oc, df_cty, cty,max_free =2)
})

result_df_oc <- bind_rows(purrr::map(results_list_conso_oc, "results"))
loading_df_oc <- bind_rows(purrr::map(results_list_conso_oc, "loadings"))


# plot loaing with country names:

summary_df <- loading_df_oc %>%
  group_by(item,country) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
  )

loading_oc_plot <- ggplot(summary_df, aes(x = item, y = mean_loading)) +
  geom_text_repel(aes(label = country), size = 4, force = 1, max.overlaps = Inf) +
  labs(
    title = "Openness to change",
    x = "Item",
    y = "Standardized Loading"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 16),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 16)
        )
loading_oc_plot

######CS#########
results_list_conso_cs <- purrr::map( countries , function(cty) {
  df_cty <- combined_data_com_country_filtered %>%
    filter(cntry == cty, essround %in% 9:11) %>%
    mutate(essround = as.factor(essround))
  check_partial_scalar_invariance(model_cs, df_cty, cty,max_free =3)
})

result_df_cs <- bind_rows(purrr::map(results_list_conso_cs, "results"))
loading_df_cs <- bind_rows(purrr::map(results_list_conso_cs, "loadings"))


# plot loading with country names:

summary_df <- loading_df_cs %>%
  group_by(item,country) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
  )


loading_cs_plot <- ggplot(summary_df, aes(x = item, y = mean_loading)) +
  geom_text_repel(aes(label = country), size = 4, force = 1, max.overlaps = Inf) +
  labs(
    title = "Conservation",
    x = "Item",
    y = "Standardized Loading"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 16),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 16)
  )

loading_cs_plot

######SE#########
results_list_conso_se <- purrr::map( countries , function(cty) {
  df_cty <- combined_data_com_country_filtered %>%
    filter(cntry == cty, essround %in% 9:11) %>%
    mutate(essround = as.factor(essround))
  check_partial_scalar_invariance(model_se, df_cty, cty,max_free =3)
})

result_df_se <- bind_rows(purrr::map(results_list_conso_se, "results"))
loading_df_se <- bind_rows(purrr::map(results_list_conso_se, "loadings"))


# plot loaing with country names:
summary_df <- loading_df_se %>%
  group_by(item,country) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
  )

loading_se_plot <- ggplot(summary_df, aes(x = item, y = mean_loading)) +
  geom_text_repel(aes(label = country), size = 4, force = 1, max.overlaps = Inf) +
  labs(
    title = "Self-enhancement",
    x = "Item",
    y = "Standardized Loading"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 16),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 16)
  )

loading_se_plot

######ST#########
results_list_conso_st <- purrr::map( countries , function(cty) {
  df_cty <- combined_data_com_country_filtered %>%
    filter(cntry == cty, essround %in% 9:11) %>%
    mutate(essround = as.factor(essround))
  check_partial_scalar_invariance(model_st, df_cty, cty,max_free =2)
})

result_df_st <- bind_rows(purrr::map(results_list_conso_st, "results"))
loading_df_st <- bind_rows(purrr::map(results_list_conso_st, "loadings"))



# plot loaing with country names:
summary_df <- loading_df_st %>%
  group_by(item,country) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
  )

loading_st_plot <- ggplot(summary_df, aes(x = item, y = mean_loading)) +
  geom_text_repel(aes(label = country), size = 4, force = 1, max.overlaps = Inf) +
  labs(
    title = "Self-transcendance",
    x = "Item",
    y = "Standardized Loading"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 16),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 16)
  )



#########merge scalar invariance result of all values

merged_df_1 <- merge(result_df_st %>% select(country, scalar_status) , result_df_oc %>% select(country, scalar_status)  , by = "country", suffixes = c(".st", ".oc"))
merged_df_2 <- merge(result_df_se %>% select(country, scalar_status) , result_df_cs %>% select(country, scalar_status)  , by = "country", suffixes = c(".se", ".cs"))

merged_df_all <- merge(merged_df_1 , merged_df_2, by = 'country' )



######################Calculate factor score and analysis df##########################

create_factor_score_data <- function(value_type, results_list, result_df, combined_data) {
  
  analyzing_countries <- result_df %>%
    filter(scalar_status != 'none' & !is.na(scalar_status)) %>%
    pull(country)
  
  country_data_list <- list()
  
  for (i in analyzing_countries) {
    country_result <- results_list[[which(sapply(results_list, function(x) x$country) == i)]]
    
    if (!is.null(country_result$model) &&
        !is.na(country_result$results$scalar_status) &&
        country_result$results$scalar_status != "none") {
      
      country_data <- combined_data %>%
        filter(cntry == i, essround %in% 9:11) %>%
        mutate(essround = as.factor(essround))
      
      factor_scores <- lavPredict(country_result$model, newdata = country_data)
      all_scores <- do.call(rbind, factor_scores)
      
      country_factor <- country_data %>%
        mutate(factor_score = all_scores[, 1]) %>%
        select(mean_restype_oc,mean_restype_cs,mean_restype_st,mean_restype_se, ipcrtiv, impfree, impdiff, ipadvnt,
               cntry, essround, age_group, age_group_2, inc_group,
               gndr, factor_score, eisced, hinctnta, agea, eu_region, edu_group)
      
      country_data_list[[i]] <- country_factor
      message(paste("Processed", i, "for", value_type))
      
    } else {
      message(paste("Skipped", i, "for", value_type, "- No adequate model or scalar invariance"))
    }
  }
  
  final_data <- bind_rows(country_data_list) %>%
    mutate(
      enter_covid_effect = if_else(essround == 10, 1, 0),
      exit_covid_effect = if_else(essround == 11, 1, 0),
    ) %>%
    filter(eisced <= 10, agea <= 200,gndr <=3 )
  
  return(final_data)
}


data_oc_fin <- create_factor_score_data(
  value_type = "oc",
  results_list = results_list_conso_oc,
  result_df = result_df_oc,
  combined_data = combined_data_com_country_filtered
) %>% rename(mean_restype = mean_restype_oc)

data_cs_fin <- create_factor_score_data(
  value_type = "cs",
  results_list = results_list_conso_cs,
  result_df = result_df_cs,
  combined_data = combined_data_com_country_filtered
) %>% rename(mean_restype = mean_restype_cs)

data_st_fin <- create_factor_score_data(
  value_type = "st",
  results_list = results_list_conso_st,
  result_df = result_df_st,
  combined_data = combined_data_com_country_filtered
) %>% rename(mean_restype = mean_restype_st)

data_se_fin <- create_factor_score_data(
  value_type = "se",
  results_list = results_list_conso_se,
  result_df = result_df_se,
  combined_data = combined_data_com_country_filtered
) %>% rename(mean_restype = mean_restype_se)

data_se_fin %>% filter(cntry == 'NL') %>% slice(1:2) 

data_oc_fin %>% filter(cntry == 'NL') %>% slice(1:2) 

loading_oc_plot <- loading_oc_plot + ggtitle("Openness to change") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0))
loading_cs_plot <- loading_cs_plot + ggtitle("Conservation") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0))
loading_st_plot <- loading_st_plot + ggtitle("Self-enhancement") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0))
loading_se_plot <- loading_se_plot + ggtitle("Self-transcendance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0))


