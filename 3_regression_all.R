library(dplyr)
library(lavaan)
library(purrr)
library(fixest)
library(texreg)
library(ggplot2)
library(broom)
library(emmeans)
library(patchwork)
library(tidyr)
# List of named datasets
data_list <- list(
  openness_to_change = data_oc_fin,
  conservation = data_cs_fin,
  self_enhancement = data_se_fin,
  self_transcendence = data_st_fin
)

################ Run overall European-wide regressions ##################
models_pre <- list()
models_post <- list()

for (value_name in names(data_list)) {
  df <- data_list[[value_name]]
  
  models_pre[[value_name]] <- feols(
    factor_score ~ agea + enter_covid_effect + eisced  +gndr + mean_restype  | cntry,
    data = df %>% filter(essround %in% c(9, 10)  ) 
  )
  
  models_post[[value_name]] <- feols(
    factor_score ~ agea + exit_covid_effect + eisced   +gndr  + mean_restype | cntry,
    data = df %>% filter(essround %in% c(10, 11)  )
  )
}

# Combine all models for screenreg
screenreg(
  c(models_pre[1], models_post[1],models_pre[2], models_post[2]),
  custom.model.names = c(
    paste0(names(models_pre)[1], "_enter"),
    paste0(names(models_post[1]), "_exit"),
    paste0(names(models_pre)[2], "_enter"),
    paste0(names(models_post[2]), "_exit")
  )
  ,stars = c(0.001, 0.01, 0.05, 0.1)
)

screenreg(
  c(models_pre[3], models_post[3],models_pre[4], models_post[4]),
  custom.model.names = c(
    paste0(names(models_pre)[3], "_enter"),
    paste0(names(models_post[3]), "_exit"),
    paste0(names(models_pre)[4], "_enter"),
    paste0(names(models_post[4]), "_exit")
  )
  ,stars = c(0.001, 0.01, 0.05, 0.1)
  
)


# Function to extract coefficients
tidy_models <- function(model_list, term_name, label) {
  bind_rows(lapply(names(model_list), function(name) {
    broom::tidy(model_list[[name]], conf.int = TRUE) %>%
      filter(term == term_name) %>%
      mutate(value = name)
  })) %>% mutate(period = label)
}

# Extract both effects
coef_enter <- tidy_models(models_pre, "enter_covid_effect", "Enter (R9→10)")
coef_exit <- tidy_models(models_post, "exit_covid_effect", "Exit (R10→11)")

# Combine
coef_all <- bind_rows(coef_enter, coef_exit)

# Factor & label for clean plot
coef_all <- coef_all %>%
  mutate(
    value = factor(value,
                   levels = c("openness_to_change","conservation", "self_transcendence", "self_enhancement"),
                   labels = c("Openness", "Conservation", "Self-Transcendence", "Self-Enhancement")),
    period = factor(period, levels = c("Enter (R9→10)", "Exit (R10→11)"))
  )

# Plot
plot_regression_overall<-ggplot(coef_all, aes(x = value, y = estimate, color = period)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +  # <- this is the line
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  labs(x='',
       y = "Estimated Change",
       color = "Period"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######################regression for each european regions########################
# Function to run regressions & return tidy data
run_value_models_by_region <- function(region_name) {
  # Filter and run all models
  get_model_data <- function(df, pre_rounds, effect, model_label, stage_label) {
    formula_str <- paste0("factor_score ~ agea + ", effect, " + eisced  + gndr + mean_restype | cntry")
    
    feols(
      as.formula(formula_str),
      data = df %>% filter(essround %in% pre_rounds, eu_region == region_name)
    ) %>%
      tidy(conf.int = TRUE) %>%
      filter(term == effect) %>%
      mutate(model = model_label, stage = stage_label)
  }
  # Each model
  open_pre <- get_model_data(data_oc_fin, c(9, 10), "enter_covid_effect", "Openness", "Entering")
  open_post <- get_model_data(data_oc_fin, c(11, 10), "exit_covid_effect", "Openness", "Exiting")
  con_pre <- get_model_data(data_cs_fin, c(9, 10), "enter_covid_effect", "Conservation", "Entering")
  con_post <- get_model_data(data_cs_fin, c(11, 10), "exit_covid_effect", "Conservation", "Exiting")
  st_pre <- get_model_data(data_st_fin, c(9, 10), "enter_covid_effect", "Self-transcendence", "Entering")
  st_post <- get_model_data(data_st_fin, c(11, 10), "exit_covid_effect", "Self-transcendence", "Exiting")
  se_pre <- get_model_data(data_se_fin, c(9, 10), "enter_covid_effect", "Self-enhancement", "Entering")
  se_post <- get_model_data(data_se_fin, c(11, 10), "exit_covid_effect", "Self-enhancement", "Exiting")
  
  # Combine all
  bind_rows(open_pre, open_post, con_pre, con_post, st_pre, st_post, se_pre, se_post) %>%
    mutate(region = region_name)
}

regions <- c("W-Europe", "S-Europe", "N-Europe", "E-Europe")
conso_data_all <- bind_rows(lapply(regions, run_value_models_by_region))
conso_data_all <- conso_data_all %>%
  mutate(model = factor(model, levels = c("Openness", "Conservation", "Self-transcendence", "Self-enhancement")))

# Plot function per region
plot_by_region <- function(region_data, region_name) {
  ggplot(region_data, aes(x = model, y = estimate, color = stage)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.2,
                  position = position_dodge(width = 0.5)) +
    labs(
      title = region_name,  # Region name as subtitle
      y = "Estimate (with 95% CI)",
      x = "",
      color = "Stage"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 10) +
    coord_cartesian(ylim = c(-0.5, 0.5)) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 12)
    )
}

# Generate plots
plots_by_region <- lapply(regions, function(rg) {
  region_data <- conso_data_all %>% filter(region == rg)
  plot_by_region(region_data, rg)
})


combined_plot_regions <- (plots_by_region[[1]]  | plots_by_region[[2]] ) / (plots_by_region[[3]]  | plots_by_region[[4]]  ) +
  plot_annotation(
#    title = "Changes in Human Values During and After COVID Across European Regions",
    theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  )



# Display
print(combined_plot)





#########age interaction effect#############

age_group_summary<- combined_data_com_country_filtered %>%
  group_by(cntry, essround, age_group_2) %>%
  summarise(No_obs = n(), .groups = "drop") %>%
  unite("round_age", essround, age_group_2, sep = "_") %>%
  pivot_wider(
    names_from = round_age,
    values_from = No_obs,
    values_fill = 0
  )


get_marginal_age_effects <- function(df, value_name) {
  df_pre <- df %>%
    filter(essround %in% c(9, 10)) %>%
    rename(ag = age_group_2, enter = enter_covid_effect) %>%
    mutate(ag = relevel(factor(ag), ref = "30-59"))
  
  df_post <- df %>%
    filter(essround %in% c(10, 11)) %>%
    rename(ag = age_group_2, exit = exit_covid_effect) %>%
    mutate(ag = relevel(factor(ag), ref = "30-59"))
  
  pre_model <- feols(factor_score ~ enter * ag + eisced  + gndr +mean_restype | cntry, data = df_pre)
  post_model <- feols(factor_score ~ exit * ag + eisced  + gndr  +mean_restype | cntry, data = df_post)
  
  # Use emtrends to get slopes of 'enter' or 'exit' by age group
  pre_emtrends <- emtrends(pre_model, ~ ag, var = "enter", data = df_pre)
  post_emtrends <- emtrends(post_model, ~ ag, var = "exit", data = df_post)
  
  # Summaries with infer=TRUE to get SE and p-values per group slope
  pre_emtrends_df <- summary(pre_emtrends, infer = TRUE) %>%
    as.data.frame() %>%
    mutate(stage = "Entering", value = value_name) %>%
    rename(marginal_effect = enter.trend,
           lower.CL = lower.CL,
           upper.CL = upper.CL)
  
  post_emtrends_df <- summary(post_emtrends, infer = TRUE) %>%
    as.data.frame() %>%
    mutate(stage = "Exiting", value = value_name) %>%
    rename(marginal_effect = exit.trend,
           lower.CL = lower.CL,
           upper.CL = upper.CL)
  
  # Pairwise contrasts of slopes between groups
  pre_contrasts <- contrast(pre_emtrends, method = "revpairwise", infer = TRUE) %>%
    summary() %>%
    as.data.frame() %>%
    mutate(stage = "Entering", value = value_name)
  
  post_contrasts <- contrast(post_emtrends, method = "revpairwise", infer = TRUE) %>%
    summary() %>%
    as.data.frame() %>%
    mutate(stage = "Exiting", value = value_name)
  
  list(
    marginals = bind_rows(pre_emtrends_df, post_emtrends_df),
    comparisons = bind_rows(pre_contrasts, post_contrasts)
  )
}

all_results <- lapply(names(data_list), function(name) {
  get_marginal_age_effects(data_list[[name]], name)
})

marginal_effects <- bind_rows(lapply(all_results, `[[`, "marginals"))
pairwise_comparisons <- bind_rows(lapply(all_results, `[[`, "comparisons"))


plot_list_marg <- lapply(unique(marginal_effects$value), function(val) {
  df_val <- marginal_effects %>% filter(value == val)
  
  ggplot(df_val, aes(x = ag, y = marginal_effect, color = stage)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
#      title = val,
      x = "Age Group",
      y = "Marginal Effect of Timephase",
      color = "Stage"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)
          ,legend.position = "bottom"
          )
})

plot_list_marg[[1]] <- plot_list_marg[[1]] + ggtitle("Openness to change") +
  theme(plot.title = element_text(size = 12, face = "bold"))
plot_list_marg[[2]] <- plot_list_marg[[2]] + ggtitle("Conservation") +
  theme(plot.title = element_text(size = 12, face = "bold"))
plot_list_marg[[3]] <- plot_list_marg[[3]] + ggtitle("Self-enhancement") +
  theme(plot.title = element_text(size = 12, face = "bold"))
plot_list_marg[[4]] <- plot_list_marg[[4]] + ggtitle("Self-transcendance") +
  theme(plot.title = element_text(size = 12, face = "bold"))

combined_plot_marg_age <- (plot_list_marg[[1]]  | plot_list_marg[[2]] ) / (plot_list_marg[[3]]  | plot_list_marg[[4]]  ) +
  plot_annotation(
#    title = "Marginal Effects of COVID-19 Stage by Age Group on Human Values",
    theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  )

print(combined_plot_marg_age)




#########income interaction effect#############

income_no_obs <- combined_data_com_country_filtered %>%
  unite(essround_inc, essround, inc_group, sep = "_") %>%
  group_by(cntry, essround_inc) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = essround_inc,
    values_from = n_obs
  )


get_marginal_inc_effects <- function(df, value_name) {
  df_pre <- df %>%
    filter(essround %in% c(9, 10)) %>%
    rename(enter = enter_covid_effect) 
  
  df_post <- df %>%
    filter(essround %in% c(10, 11)) %>%
    rename(exit = exit_covid_effect) 
  
  pre_model <- feols(factor_score ~ enter * inc_group + eisced + mean_restype + gndr +agea | cntry, data = df_pre)
  post_model <- feols(factor_score ~ exit * inc_group + eisced + mean_restype + gndr  +agea | cntry, data = df_post)
  
  # Use emtrends to get slopes of 'enter' or 'exit' by age group
  pre_emtrends <- emtrends(pre_model, ~ inc_group, var = "enter", data = df_pre)
  post_emtrends <- emtrends(post_model, ~ inc_group, var = "exit", data = df_post)
  
  # Summaries with infer=TRUE to get SE and p-values per group slope
  pre_emtrends_df <- summary(pre_emtrends, infer = TRUE) %>%
    as.data.frame() %>%
    mutate(stage = "Entering", value = value_name) %>%
    rename(marginal_effect = enter.trend,
           lower.CL = lower.CL,
           upper.CL = upper.CL)
  
  post_emtrends_df <- summary(post_emtrends, infer = TRUE) %>%
    as.data.frame() %>%
    mutate(stage = "Exiting", value = value_name) %>%
    rename(marginal_effect = exit.trend,
           lower.CL = lower.CL,
           upper.CL = upper.CL)
  
  # Pairwise contrasts of slopes between groups
  pre_contrasts <- contrast(pre_emtrends, method = "revpairwise", infer = TRUE) %>%
    summary() %>%
    as.data.frame() %>%
    mutate(stage = "Entering", value = value_name)
  
  post_contrasts <- contrast(post_emtrends, method = "revpairwise", infer = TRUE) %>%
    summary() %>%
    as.data.frame() %>%
    mutate(stage = "Exiting", value = value_name)
  
  list(
    marginals = bind_rows(pre_emtrends_df, post_emtrends_df),
    comparisons = bind_rows(pre_contrasts, post_contrasts)
  )
}

all_results_inc <- lapply(names(data_list), function(name) {
  get_marginal_inc_effects(data_list[[name]], name)
})

marginal_effects_inc <- bind_rows(lapply(all_results_inc, `[[`, "marginals"))
pairwise_comparisons_inc <- bind_rows(lapply(all_results_inc, `[[`, "comparisons"))


plot_list_marg_inc <- lapply(unique(marginal_effects_inc$value), function(val) {
  df_val <- marginal_effects_inc %>% filter(value == val)
  
  ggplot(df_val, aes(x = inc_group, y = marginal_effect, color = stage)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
#      title = val,
      x = "Income Group",
      y = "Marginal Effect of Timephase",
      color = "Stage"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)
          ,legend.position = "bottom"
          )
})

plot_list_marg_inc[[1]] <- plot_list_marg_inc[[1]] + ggtitle("Openness to change") +
  theme(plot.title = element_text(size = 12, face = "bold"))
plot_list_marg_inc[[2]] <- plot_list_marg_inc[[2]] + ggtitle("Conservation") +
  theme(plot.title = element_text(size = 12, face = "bold"))
plot_list_marg_inc[[3]] <- plot_list_marg_inc[[3]] + ggtitle("Self-enhancement") +
  theme(plot.title = element_text(size = 12, face = "bold"))
plot_list_marg_inc[[4]] <- plot_list_marg_inc[[4]] + ggtitle("Self-transcendance") +
  theme(plot.title = element_text(size = 12, face = "bold"))

combined_plot_marg_inc <- (plot_list_marg_inc[[1]]  | plot_list_marg_inc[[2]] ) / (plot_list_marg_inc[[3]]  | plot_list_marg_inc[[4]]  ) +
  plot_annotation(
#    title = "Marginal Effects of COVID-19 Stage by Age Group on Human Values",
    theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  )

print(combined_plot_marg_inc)



