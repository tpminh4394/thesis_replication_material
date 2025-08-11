library(dplyr)
library(ggplot2)
library(lavaan)
library(tidyr)
library(tibble)
library(purrr)
####Load data####
#Load data from the ESS website here 
ess9 
ess10 
ess11 


####Find common countries####

common_countries_x <- Reduce(intersect, list(unique(ess9$cntry), unique(ess10$cntry), unique(ess11$cntry)))
common_countries_x
common_countries <- c( "FI", "HR", "HU", "NO")

####Process data####

filter_ess9 <- ess9 %>%
  select(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ipshabt, 
         ipsuces, imprich, iprspot, impsafe, ipstrgv, ipfrule, ipbhprp, 
         imptrad, ipmodst, iphlppl, iplylfr, ipeqopt, ipudrst, impenv, cntry, essround, anweight,dweight, agea, gndr,edulvlb,hinctnta,eisced
         , ppltrst,pplfair,pplhlp,freehms,hmsfmlsh,hmsacld) %>%
  #  filter(cntry %in% c('IE', 'GB', 'NO', 'CZ', 'HU')) %>%
  filter(across(c(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ipshabt, 
                  ipsuces, imprich, iprspot, impsafe, ipstrgv, ipfrule, ipbhprp, 
                  imptrad, ipmodst, iphlppl, iplylfr, ipeqopt, ipudrst, impenv), ~ . %in% c(1, 2, 3, 4, 5, 6))) 
#  filter(agea != 999) %>%   filter(gndr !=  9)   %>%drop_na() 



filter_ess10 <- ess10 %>%
  select(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ipshabt, 
         ipsuces, imprich, iprspot, impsafe, ipstrgv, ipfrule, ipbhprp, 
         imptrad, ipmodst, iphlppl, iplylfr, ipeqopt, ipudrst, impenv, cntry, essround, anweight,dweight, agea, gndr,edulvlb,hinctnta,eisced
         , ppltrst,pplfair,pplhlp,freehms,hmsfmlsh,hmsacld) %>%
  #  filter(cntry %in% c('IE', 'GB', 'NO', 'CZ', 'HU')) %>%
  filter(across(c(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ipshabt, 
                  ipsuces, imprich, iprspot, impsafe, ipstrgv, ipfrule, ipbhprp, 
                  imptrad, ipmodst, iphlppl, iplylfr, ipeqopt, ipudrst, impenv), ~ . %in% c(1, 2, 3, 4, 5, 6))) 
#  filter(agea != 999) %>%   filter(gndr !=  9)   %>%drop_na() 

filter_ess11 <- ess11 %>%
  select(ipcrtiva, impfreea, impdiffa, ipadvnta, ipgdtima, impfuna, ipshabta, 
         ipsucesa, impricha, iprspota, impsafea, ipstrgva, ipfrulea, ipbhprpa, 
         imptrada, ipmodsta, iphlppla, iplylfra, ipeqopta, ipudrsta, impenva, cntry, essround, anweight,dweight, agea, gndr,edulvlb,hinctnta,eisced
         , ppltrst,pplfair,pplhlp,freehms,hmsfmlsh,hmsacld) %>%
  #  filter(cntry %in% c('IE', 'GB', 'NO', 'CZ', 'HU')) %>%
  rename(
    ipcrtiv = ipcrtiva,
    impfree = impfreea,
    impdiff = impdiffa,
    ipadvnt = ipadvnta,
    ipgdtim = ipgdtima,
    impfun = impfuna,
    ipshabt = ipshabta,
    ipsuces = ipsucesa,
    imprich = impricha,
    iprspot = iprspota,
    impsafe = impsafea,
    ipstrgv = ipstrgva,
    ipfrule = ipfrulea,
    ipbhprp = ipbhprpa,
    imptrad = imptrada,
    ipmodst = ipmodsta,
    iphlppl = iphlppla,
    iplylfr = iplylfra,
    ipeqopt = ipeqopta,
    ipudrst = ipudrsta,
    impenv = impenva
  ) %>%
  filter(across(c(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ipshabt, 
                  ipsuces, imprich, iprspot, impsafe, ipstrgv, ipfrule, ipbhprp, 
                  imptrad, ipmodst, iphlppl, iplylfr, ipeqopt, ipudrst, impenv), ~ . %in% c(1, 2, 3, 4, 5, 6))) 
#  filter(agea != 999) %>%   filter(gndr !=  9)   %>%drop_na() 


ess9_full <- ess9 %>% select(cntry, essround) 
ess10_full <- ess10 %>% select(cntry, essround) 
ess11_full <- ess11 %>% select(cntry, essround) 
combined_data_full <- bind_rows( ess9_full, ess10_full,ess11_full)


combined_data <- bind_rows( filter_ess9, filter_ess10,filter_ess11)

# View the combined dataset
head(combined_data)

combined_data_com_country <- combined_data %>% 
  filter(cntry %in% common_countries_x) 

pvq_items <- combined_data_com_country[, 1:21]

# Function to count how many times the most frequent answer occurs
max_repeated_count <- apply(pvq_items, 1, function(x) {
  max(table(x))
})

# Add it to the data frame
combined_data_com_country$max_repeated <- max_repeated_count

# Filter: keep only those with fewer than 16 identical responses
combined_data_com_country_filtered_org <- combined_data_com_country[combined_data_com_country$max_repeated < 17, ]

#Add age group
combined_data_com_country_filtered_org <- combined_data_com_country_filtered_org %>% mutate(
  age_group = case_when(
    agea < 18 ~ "<18",
    agea >= 18 & agea <= 25 ~ "18-25",
    agea > 25 & agea <= 31 ~ "26-31",
    agea > 31 & agea <= 40 ~ "32-40",
    agea > 40 & agea <= 50 ~ "41-50",
    agea > 50 & agea <= 60 ~ "51-60",
    agea > 60 & agea <= 200 ~ "60+",
    TRUE ~ NA_character_
  ),
  age_group_2 = case_when(
    agea < 25 ~ "<30",
    agea >= 30 & agea <= 59 ~ "30-59",
    agea >= 60 & agea <= 150 ~ ">=60",
    TRUE ~ NA_character_
  ),
  inc_group = case_when(
    hinctnta %in% c(1,2) ~ ('lower_income'),
    hinctnta %in% c(3,4,5,6,7,8,9,10) ~('the_rest'),
    TRUE ~ NA_character_
  ),
  eu_region = case_when(
    cntry %in% c('IE', 'IS', 'FI', 'NO', 'GB') ~ 'N-Europe',
    cntry %in% c('BE', 'FR', 'NL', 'CH') ~ 'W-Europe',
    cntry %in% c( 'IT', 'PT') ~ 'S-Europe',
    cntry %in% c( 'HU', 'LT', 'SI', 'SK', 'HR') ~ 'E-Europe'
  )
  ,
  edu_group = case_when(
    eisced <= 5 ~ '1. No College Degree',
    eisced == 6 ~ '2. College Degree',
    eisced == 7 ~ '3. Graduate Degree',
  ),
  
)

combined_data_com_country_filtered_org %>%
  group_by(inc_group) %>%
  summarise(n_before = n())

#######Summary table#############

# Count respondents by country and round BEFORE filtering
before_filtering <- combined_data_full %>%
  group_by(cntry, essround) %>%
  summarise(obs_before = n(), .groups = "drop")

# Count respondents by country and round AFTER filtering
after_filtering <- combined_data_com_country_filtered_org %>%
  group_by(cntry, essround) %>%
  summarise(obs_after = n(), .groups = "drop")

# Combine before and after
counts_combined <- full_join(before_filtering, after_filtering, by = c("cntry", "essround"))

# Replace NA with 0
counts_combined[is.na(counts_combined)] <- 0

# Pivot to wide format: each round gets two columns: before and after
respondent_counts_wide <- counts_combined %>%
  pivot_wider(
    names_from = essround,
    values_from = c(obs_before, obs_after),
    names_glue = "round{essround}_{.value}"
  ) %>% filter(cntry %in% c(common_countries_x)) %>%
  select(cntry,round9_obs_before,round10_obs_before, round11_obs_before,  round9_obs_after,round10_obs_after ,round11_obs_after)

