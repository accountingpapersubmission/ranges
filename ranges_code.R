suppressMessages({
library("tidyverse")    
library("modelsummary") 
library("flextable")    
library("ggplot2")      
library("readxl")       
library("lubridate")    
library("fixest")       
library("broom")        
library("rempsyc")   
})

################################################################################
##### Read in data #############################################################
################################################################################

responses <- read.csv("./data/ranges_forecasts_raw_data.csv", comment.char="#")

industries <- read_excel("./data/range_forecasts_industries.xlsx", sheet = "Tabelle1")

################################################################################
##### Merge data and variable preparation ######################################
################################################################################

df <- left_join(responses, industries, by = "ExternalReference") %>%
  mutate(progress = as.numeric(Progress),
         duration = as.numeric(Duration..in.seconds.)/60, #in minutes
         start = as_datetime(StartDate, format = "%Y-%m-%d %H:%M:%S"),
         end = as_datetime(EndDate, format = "%Y-%m-%d %H:%M:%S"),
         session_start = case_when(
           start <= as_datetime("2022-01-11 07:57:00", format = "%Y-%m-%d %H:%M:%S") ~ 1,
           start > as_datetime("2022-01-11 07:57:00", format = "%Y-%m-%d %H:%M:%S") &
             start <= as_datetime("2022-01-25 09:01:00", format = "%Y-%m-%d %H:%M:%S") ~ 2,
           start > as_datetime("2022-01-25 09:01:00", format = "%Y-%m-%d %H:%M:%S") ~ 3),
         session_end = case_when(
           end <= as_datetime("2022-01-11 07:57:00", format = "%Y-%m-%d %H:%M:%S") ~ 1,
           end > as_datetime("2022-01-11 07:57:00", format = "%Y-%m-%d %H:%M:%S") &
             end <= as_datetime("2022-01-25 09:01:00", format = "%Y-%m-%d %H:%M:%S") ~ 2,
           end > as_datetime("2022-01-25 09:01:00", format = "%Y-%m-%d %H:%M:%S") ~ 3),
         late_responder = ifelse(session_end == 1, 0, 1),
         true_belief_1 = as.numeric(gsub(",",".",(sub("\\.", "", TrueBeliefsV1_1)))),
         true_belief_2 = as.numeric(gsub(",",".",TrueBeliefsV2_1)),
         wide_range = ifelse(is.na(true_belief_1),1,0),
         true_belief = ifelse(is.na(true_belief_1), true_belief_2, true_belief_1),
         conf_int_1 = as.numeric(gsub(",",".",gsub("%","",ConfidenceIntervalV1_1))),
         conf_int_2 = as.numeric(ConfidenceIntervalV2_1),
         conf_int = ifelse(is.na(conf_int_1),conf_int_2,conf_int_1),
         em_accrual = as.numeric(ifelse(is.na(as.numeric(EarningsManagementV1_1)), 
                                        EarningsManagementV2_1, EarningsManagementV1_1)),
         em_real = as.numeric(ifelse(is.na(as.numeric(EarningsManagementV1_2)), 
                                     EarningsManagementV2_2, EarningsManagementV1_2)),
         inf_range = as.numeric(ifelse(is.na(as.numeric(InformativenessV1_1)), 
                                       InformativenessV2_1, InformativenessV1_1)),
         inf_point = as.numeric(ifelse(is.na(as.numeric(InformativenessV1_2)), 
                                       InformativenessV2_2, InformativenessV1_2)),
         inf_mid = as.numeric(ifelse(is.na(as.numeric(InformativenessV1_3)), 
                                     InformativenessV2_3, InformativenessV1_3)),
         inf_min = as.numeric(ifelse(is.na(as.numeric(InformativenessV1_4)), 
                                     InformativenessV2_4, InformativenessV1_4)),
         inf_max = as.numeric(ifelse(is.na(as.numeric(InformativenessV1_5)), 
                                     InformativenessV2_5, InformativenessV1_5)),
         inf_open = ifelse(is.na(inf_min),inf_max,inf_min),
         group = case_when(as.numeric(Konzernabschluss) == 2 ~ NA_real_, # NA 
                           as.numeric(Konzernabschluss) == 0 ~ 1, # Yes
                           as.numeric(Konzernabschluss) == 1 ~ 0), # No
         public = case_when(as.numeric(Kapitalmarkt) == 2 ~ NA_real_, #NA
                            as.numeric(Kapitalmarkt) == 0 ~ 1, # Yes
                            as.numeric(Kapitalmarkt) == 1 ~ 0), #No
         sales_unt_text = as.numeric(gsub("\\.","",
                                          gsub("40 mio","40000000",
                                               gsub("17.721.101,92","17721102",
                                                    gsub("1,8 Mio","1800000",SalesUnternehmen_0_TEXT))))),
         sales_unt_num = case_when(
           as.numeric(SalesUnternehmen) == 10 ~ NA_real_,
           TRUE ~ as.numeric(SalesUnternehmen)),
         sales_unt = case_when(
           is.na(sales_unt_text) ~ sales_unt_num,
           sales_unt_text <= 10000000 ~ 1,
           sales_unt_text > 10000000 & sales_unt_text <= 12000000 ~ 2,
           sales_unt_text > 12000000 & sales_unt_text <= 20000000 ~ 3,
           sales_unt_text > 20000000 & sales_unt_text <= 40000000 ~ 4,
           sales_unt_text > 40000000 & sales_unt_text <= 50000000 ~ 5,
           sales_unt_text > 50000000 & sales_unt_text <= 100000000 ~ 6,
           sales_unt_text > 100000000 & sales_unt_text <= 500000000 ~ 7,
           sales_unt_text > 500000000 & sales_unt_text <= 1000000000 ~ 8,
           sales_unt_text > 1000000000 ~ 9),
         sales_group_text = as.numeric(gsub("\\.","",
                                            gsub("340.430.322,07","340430322",SalesKonzern_0_TEXT))),
         sales_group_num = case_when(
           as.numeric(SalesKonzern) == 12 ~ NA_real_,
           as.numeric(SalesKonzern) >= 9 & as.numeric(SalesKonzern) <= 11 ~ 9,
           TRUE ~ as.numeric(SalesKonzern)),
         sales_group = case_when(
           is.na(sales_group_text) ~ sales_group_num,
           sales_group_text <= 10000000 ~ 1,
           sales_group_text > 10000000 & sales_group_text <= 12000000 ~ 2,
           sales_group_text > 12000000 & sales_group_text <= 20000000 ~ 3,
           sales_group_text > 20000000 & sales_group_text <= 40000000 ~ 4,
           sales_group_text > 40000000 & sales_group_text <= 50000000 ~ 5,
           sales_group_text > 50000000 & sales_group_text <= 100000000 ~ 6,
           sales_group_text > 100000000 & sales_group_text <= 500000000 ~ 7,
           sales_group_text > 500000000 & sales_group_text <= 1000000000 ~ 8,
           sales_group_text > 1000000000 ~ 9),
         sales = ifelse(group == 1, sales_group, sales_unt),
         emp_unt_text = as.numeric(EmployeesUnternehmen_0_TEXT),
         emp_unt_num = case_when(
           as.numeric(EmployeesUnternehmen) == 7 ~ NA_real_,
           TRUE ~ as.numeric(EmployeesUnternehmen)),
         emp_unt = case_when(
           is.na(emp_unt_text) ~ emp_unt_num,
           emp_unt_text <= 50 ~ 1,
           emp_unt_text > 50 & emp_unt_text <= 250 ~ 2,
           emp_unt_text > 250 & emp_unt_text <= 1000 ~ 3,
           emp_unt_text > 1000 & emp_unt_text <= 5000 ~ 4,
           emp_unt_text > 5000 & emp_unt_text <= 10000 ~ 5, # No unt. has more than 5000, so can be combined with groups
           emp_unt_text > 10000 ~ 6),
         emp_group_text = as.numeric(gsub("\\.","",EmployeesKonzern_0_TEXT)),
         emp_group_num = case_when(
           as.numeric(EmployeesKonzern) == 10 ~ NA_real_,
           TRUE ~ as.numeric(EmployeesKonzern)),
         emp_group = case_when(
           is.na(emp_group_text) ~ emp_group_num,
           emp_group_text <= 50 ~ 1,
           emp_group_text > 50 & emp_group_text <= 250 ~ 2,
           emp_group_text > 250 & emp_group_text <= 1000 ~ 3,
           emp_group_text > 1000 & emp_group_text <= 5000 ~ 4,
           emp_group_text > 5000 & emp_group_text <= 10000 ~ 5,
           emp_group_text > 10000 & emp_group_text <= 25000 ~ 6,
           emp_group_text > 25000 & emp_group_text <= 50000 ~ 7,
           emp_group_text > 50000 & emp_group_text <= 100000 ~ 8,
           emp_group_text > 100000 ~ 9),
         employees = ifelse(group == 1, emp_group, emp_unt),
         work_experience = case_when(
           Working.experience == 1 ~ NA_real_,
           TRUE ~ as.numeric(gsub("3 6","36",Working.experience_0_TEXT))),
         pessimism_1 = case_when(
           PessimismOptimism1 == 1 ~ -1,
           PessimismOptimism1 == 2 ~ 1,
           PessimismOptimism1 == 3 ~ 0),
         pessimism_2 = as.numeric(PessimismOptimism2) - 3,
         position = case_when(
           Position == 1 ~ "Head of Accounting",
           Position == 2 ~ "Accounting Staff",
           Position == 3 ~ "Head of Controlling",
           Position == 4 ~ "Controlling Staff",
           Position == 5 ~ "Head of Finance",
           Position == 6 ~ "Finance Staff",
           Position == 7 ~ "CFO",
           Position == 8 ~ NA_character_,
           Position == "" ~ NA_character_,
           Position == 0 & (Position_0_TEXT == "CEO" |
                              startsWith(Position_0_TEXT, "gesch") |
                              startsWith(Position_0_TEXT, "Gesch") |
                              startsWith(Position_0_TEXT, "Senior")) ~ "CEO",
           TRUE ~ "Other"),
         position_text = Position_0_TEXT,
         topmanager = case_when(
           position == "Head of Accounting" ~ 1,
           position == "Head of Controlling" ~ 1,
           position == "Head of Finance" ~ 1,
           position == "CFO" ~ 1,
           position == "CEO" ~ 1,
           TRUE ~ 0),
         acct_experience = case_when(
           AccountingExperience == 4 ~ NA_real_,
           AccountingExperience == 3 ~ 0, # No involvement
           AccountingExperience == 2 ~ 1, # Indirect involvement
           AccountingExperience == 1 ~ 2), # Direct involvement
         forecast_experience = case_when(
           ForecasteExperience == 4 ~ NA_real_,
           ForecasteExperience == 3 ~ 0, # No involvement
           ForecasteExperience == 2 ~ 1, # Indirect involvement
           ForecasteExperience == 1 ~ 2), # Direct involvement
         age = case_when(
           Age == 1 ~ NA_real_,
           TRUE ~ as.numeric(Age_0_TEXT)),
         gender = case_when(
           Gender == 4 ~ NA_real_,
           TRUE ~ as.numeric(Gender))-1,
         country = ifelse(substr(ExternalReference,1,2) == "DE",0,1),
         industry = Industry,
         range_pos = case_when(
           wide_range == 0 ~ (true_belief-7.22)/(7.38-7.22),
           wide_range == 1 ~ (true_belief-6.40)/(8.20-6.40))) %>%
  select(progress,
         duration,
         session_start,
         session_end,
         late_responder,
         wide_range,
         true_belief,
         conf_int,
         em_accrual,
         em_real,
         inf_range,
         inf_point,
         inf_mid,
         inf_min,
         inf_max,
         inf_open,
         pessimism_1,
         pessimism_2,
         group,
         public,
         sales,
         employees,
         work_experience,
         acct_experience,
         forecast_experience,
         position,
         position_text,
         topmanager,
         age,
         gender,
         country,
         industry,
         range_pos)

################################################################################
##### Clean data ###############################################################
################################################################################

df <- df %>%
  filter(!(is.na(pessimism_1))) %>%
  filter(!(is.na(pessimism_2))) %>%
  filter(!is.na(true_belief)) %>%
  filter(!is.na(conf_int)) %>%
  filter(!is.na(em_real)) %>%
  filter(!is.na(em_accrual)) %>%
  filter(!is.na(inf_range)) %>%
  filter(!is.na(inf_point)) %>%
  filter(!is.na(inf_mid)) %>%
  filter(!is.na(inf_open)) %>%
  filter(duration >= 3) %>%
  filter(true_belief < 500) %>%
  filter((acct_experience == 1 | acct_experience == 2 | forecast_experience == 1 | forecast_experience == 2))

################################################################################
##### Table 2 Panel A ##########################################################
################################################################################

table_2_panel_a <- datasummary(data = df,
                               group +
                                 public +
                                 work_experience +
                                 acct_experience +
                                 forecast_experience +
                                 age +
                                 gender +
                                 country
                               ~
                                 1 + 
                                 N + 
                                 min*Arguments(na.rm = T) +
                                 P25*Arguments(na.rm = T) +
                                 median*Arguments(na.rm = T) + 
                                 mean*Arguments(na.rm = T) + 
                                 sd*Arguments(na.rm = T) +
                                 P75*Arguments(na.rm=T) +
                                 max*Arguments(na.rm = T))

table_2_panel_a

################################################################################
##### Table 2 Panel B ##########################################################
################################################################################

table_2_panel_b <- datasummary(data = df,
                               as.factor(employees) + as.factor(sales) + 
                                 employees + sales + position
                               ~ N + Percent(),
                               fmt = 1)

table_2_panel_b

################################################################################
##### Table 2 Panel C ##########################################################
################################################################################

table_2_panel_c <- datasummary_correlation(data = df %>%
                                             select(age,
                                                    gender,
                                                    country,
                                                    public,
                                                    sales,
                                                    employees,
                                                    work_experience,
                                                    forecast_experience,
                                                    wide_range),
                                           method = "pearspear",
                                           fmt = 3)

table_2_panel_c

################################################################################
##### Table 3 Panel A ##########################################################
################################################################################

##### Point ####################################################################

df %>% select(inf_point) %>% count()
df %>% summarise(mean(inf_point))
df %>% summarise(sd(inf_point))
t.test(df %>% select(inf_point), 
       mu = 4, 
       alternative = "two.sided")
t.test(df %>% select(inf_point), 
       df %>% select(inf_range), 
       alternative = "two.sided")
t.test(df %>% select(inf_point), 
       df %>% select(inf_mid),
       alternative = "two.sided")
t.test(df %>% select(inf_point), 
       df %>% select(inf_open),
       alternative = "two.sided")
datasummary_skim(df %>% 
                   select(inf_point) %>%
                   mutate(inf_point = as.factor(inf_point)),
                 type = "categorical",
                 fmt = 2)
t.test(df %>% filter(wide_range == 0) %>% select(inf_point), 
       df %>% filter(wide_range == 1) %>% select(inf_point), 
       alternative = "two.sided")

##### Range ####################################################################

df %>% select(inf_range) %>% count()
df %>% summarise(mean(inf_range))
df %>% summarise(sd(inf_range))
t.test(df %>% select(inf_range), 
       mu = 4, 
       alternative = "two.sided")
t.test(df %>% select(inf_range), 
       df %>% select(inf_point), 
       alternative = "two.sided")
t.test(df %>% select(inf_range), 
       df %>% select(inf_mid),
       alternative = "two.sided")
t.test(df %>% select(inf_range), 
       df %>% select(inf_open),
       alternative = "two.sided")
datasummary_skim(df %>% 
                   select(inf_range) %>%
                   mutate(inf_range = as.factor(inf_range)),
                 type = "categorical",
                 fmt = 2)
t.test(df %>% filter(wide_range == 0) %>% select(inf_range), 
       df %>% filter(wide_range == 1) %>% select(inf_range), 
       alternative = "two.sided")

##### Hybrid ###################################################################

df %>% select(inf_mid) %>% count()
df %>% summarise(mean(inf_mid))
df %>% summarise(sd(inf_mid))
t.test(df %>% select(inf_mid), 
       mu = 4, 
       alternative = "two.sided")
t.test(df %>% select(inf_mid), 
       df %>% select(inf_point), 
       alternative = "two.sided")
t.test(df %>% select(inf_mid), 
       df %>% select(inf_range),
       alternative = "two.sided")
t.test(df %>% select(inf_mid), 
       df %>% select(inf_open),
       alternative = "two.sided")
datasummary_skim(df %>% 
                   select(inf_mid) %>%
                   mutate(inf_mid = as.factor(inf_mid)),
                 type = "categorical",
                 fmt = 2)
t.test(df %>% filter(wide_range == 0) %>% select(inf_mid), 
       df %>% filter(wide_range == 1) %>% select(inf_mid), 
       alternative = "two.sided")

##### Open #####################################################################

df %>% select(inf_open) %>% count()
df %>% summarise(mean(inf_open))
df %>% summarise(sd(inf_open))
t.test(df %>% select(inf_open), 
       mu = 4, 
       alternative = "two.sided")
t.test(df %>% select(inf_open), 
       df %>% select(inf_point), 
       alternative = "two.sided")
t.test(df %>% select(inf_open), 
       df %>% select(inf_range),
       alternative = "two.sided")
t.test(df %>% select(inf_open), 
       df %>% select(inf_mid),
       alternative = "two.sided")
datasummary_skim(df %>% 
                   select(inf_open) %>%
                   mutate(inf_open = as.factor(inf_open)),
                 type = "categorical",
                 fmt = 2)
t.test(df %>% filter(wide_range == 0) %>% select(inf_open), 
       df %>% filter(wide_range == 1) %>% select(inf_open), 
       alternative = "two.sided")

################################################################################
##### Table 3 Panel B ##########################################################
################################################################################

table(df$pessimism_1)
t.test(df$pessimism_1, mu = 0, alternative = "two.sided")
sd(df$pessimism_1)

################################################################################
##### Table 3 Panel C ##########################################################
################################################################################

table(df$pessimism_2)
t.test(df$pessimism_2, mu = 0, alternative = "two.sided")
sd(df$pessimism_2)

################################################################################
##### Table 4 Panel A ##########################################################
################################################################################

##### Full Sample ##############################################################

df %>% filter(wide_range == 0 | wide_range == 1) %>% select(em_accrual) %>% count()
df %>% filter(wide_range == 0 | wide_range == 1) %>% summarise(mean(em_accrual))
df %>% filter(wide_range == 0 | wide_range == 1) %>% summarise(sd(em_accrual))
t.test(df %>% filter(wide_range == 0 | wide_range == 1) %>% select(em_accrual), 
       mu = 4, 
       alternative = "two.sided")
datasummary_skim(df %>% 
                   filter(wide_range == 0 | wide_range == 1) %>% 
                   select(em_accrual) %>%
                   mutate(em_accrual = as.factor(em_accrual)),
                 type = "categorical",
                 fmt = 2)
t.test(df %>% filter(wide_range == 0) %>% select(em_accrual), 
       df %>% filter(wide_range == 1) %>% select(em_accrual), 
       alternative = "two.sided")

##### Narrow range #############################################################

df %>% filter(wide_range == 0) %>% select(em_accrual) %>% count()
df %>% filter(wide_range == 0) %>% summarise(mean(em_accrual))
df %>% filter(wide_range == 0) %>% summarise(sd(em_accrual))
t.test(df %>% filter(wide_range == 0) %>% select(em_accrual), 
       mu = 4, 
       alternative = "two.sided")
datasummary_skim(df %>% 
                   filter(wide_range == 0) %>% 
                   select(em_accrual) %>%
                   mutate(em_accrual = as.factor(em_accrual)),
                 type = "categorical",
                 fmt = 2)

##### Wide range ###############################################################

df %>% filter(wide_range == 1) %>% select(em_accrual) %>% count()
df %>% filter(wide_range == 1) %>% summarise(mean(em_accrual))
df %>% filter(wide_range == 1) %>% summarise(sd(em_accrual))
t.test(df %>% filter(wide_range == 1) %>% select(em_accrual), 
       mu = 4, 
       alternative = "two.sided")
datasummary_skim(df %>% 
                   filter(wide_range == 1) %>% 
                   select(em_accrual) %>%
                   mutate(em_accrual = as.factor(em_accrual)),
                 type = "categorical",
                 fmt = 2)

################################################################################
##### Table 4 Panel B ##########################################################
################################################################################

##### Full Sample ##############################################################

df %>% filter(wide_range == 0 | wide_range == 1) %>% select(em_real) %>% count()
df %>% filter(wide_range == 0 | wide_range == 1) %>% summarise(mean(em_real))
df %>% filter(wide_range == 0 | wide_range == 1) %>% summarise(sd(em_real))
t.test(df %>% filter(wide_range == 0 | wide_range == 1) %>% select(em_real), 
       mu = 4, 
       alternative = "two.sided")
datasummary_skim(df %>% 
                   filter(wide_range == 0 | wide_range == 1) %>% 
                   select(em_real) %>%
                   mutate(em_real = as.factor(em_real)),
                 type = "categorical",
                 fmt = 2)
t.test(df %>% filter(wide_range == 0) %>% select(em_real), 
       df %>% filter(wide_range == 1) %>% select(em_real), 
       alternative = "two.sided")

##### Narrow range #############################################################

df %>% filter(wide_range == 0) %>% select(em_real) %>% count()
df %>% filter(wide_range == 0) %>% summarise(mean(em_real))
df %>% filter(wide_range == 0) %>% summarise(sd(em_real))
t.test(df %>% filter(wide_range == 0) %>% select(em_real), 
       mu = 4, 
       alternative = "two.sided")
datasummary_skim(df %>% 
                   filter(wide_range == 0) %>% 
                   select(em_real) %>%
                   mutate(em_real = as.factor(em_real)),
                 type = "categorical",
                 fmt = 2)

##### Wide range ###############################################################

df %>% filter(wide_range == 1) %>% select(em_real) %>% count()
df %>% filter(wide_range == 1) %>% summarise(mean(em_real))
df %>% filter(wide_range == 1) %>% summarise(sd(em_real))
t.test(df %>% filter(wide_range == 1) %>% select(em_real), 
       mu = 4, 
       alternative = "two.sided")
datasummary_skim(df %>% 
                   filter(wide_range == 1) %>% 
                   select(em_real) %>%
                   mutate(em_real = as.factor(em_real)),
                 type = "categorical",
                 fmt = 2)

################################################################################
##### Table 4 Panel C ##########################################################
################################################################################

t.test(df %>% filter(wide_range == 0 | wide_range == 1) %>% select(em_accrual), 
       df %>% filter(wide_range == 0 | wide_range == 1) %>% select(em_real), 
       alternative = "two.sided")

t.test(df %>% filter(wide_range == 0) %>% select(em_accrual), 
       df %>% filter(wide_range == 0) %>% select(em_real), 
       alternative = "two.sided")

t.test(df %>% filter(wide_range == 1) %>% select(em_accrual), 
       df %>% filter(wide_range == 1) %>% select(em_real), 
       alternative = "two.sided")

################################################################################
##### Table 5 Panel A ##########################################################
################################################################################

##### Full Sample ##############################################################

df %>% select(true_belief) %>% count()
t.test(df$true_belief, mu = 7.3, alternative = "two.sided")
t.test(df$range_pos, mu = 1, alternative = "two.sided") # To test whether sign. smaller than upper bound
sd(df$true_belief)
df %>% summarise(mean(range_pos, na.rm = T))
df %>%
  mutate(indicator = case_when(
    wide_range == 0 & (true_belief < 7.22 | true_belief > 7.38) ~ 1,
    wide_range == 1 & (true_belief < 6.40 | true_belief > 8.20) ~ 1,
    TRUE ~ 0
  )) %>%
  select(indicator) %>%
  count(indicator == 1)

##### Narrow ranges ############################################################

df %>% filter(wide_range == 0) %>% select(true_belief) %>% count()
t.test(df %>% filter(wide_range == 0) %>% select(true_belief), mu = 7.3, alternative = "two.sided")
t.test(df %>% filter(wide_range == 0) %>% select(true_belief), mu = 7.38, alternative = "two.sided")
t.test(df %>% filter(wide_range == 0) %>% select(range_pos), mu = 0.75, alternative = "two.sided") 
df %>% filter(wide_range == 0) %>% summarise(sd(true_belief))
df %>% filter (wide_range == 0) %>% summarise(mean(range_pos))
df %>%
  filter(wide_range == 0) %>%
  mutate(indicator = case_when(
    (true_belief < 7.22 | true_belief > 7.38) ~ 1,
    TRUE ~ 0
  )) %>%
  select(indicator) %>%
  count(indicator == 1)

##### Wide ranges ##############################################################

df %>% filter(wide_range == 1) %>% select(true_belief) %>% count()
t.test(df %>% filter(wide_range == 1) %>% select(true_belief), mu = 7.3, alternative = "two.sided")
t.test(df %>% filter(wide_range == 1) %>% select(true_belief), mu = 8.20, alternative = "two.sided")
t.test(df %>% filter(wide_range == 1) %>% select(range_pos), mu = 0.75, alternative = "two.sided") # To test whether sign. smaller than upper bound
df %>% filter(wide_range == 1) %>% summarise(sd(true_belief))
df %>% filter (wide_range == 1) %>% summarise(mean(range_pos))
df %>%
  filter(wide_range == 1) %>%
  mutate(indicator = case_when(
    (true_belief < 6.40 | true_belief > 8.20) ~ 1,
    TRUE ~ 0
  )) %>%
  select(indicator) %>%
  count(indicator == 1)

################################################################################
##### Table 5 Panel B ##########################################################
################################################################################

##### Full Sample ##############################################################

df %>% filter(wide_range == 1 | wide_range == 0) %>% select(conf_int) %>% count()
df %>% filter(wide_range == 1 | wide_range == 0) %>% select(conf_int) %>% summarise(mean(conf_int))
df %>% filter(wide_range == 1 | wide_range == 0) %>% select(conf_int) %>% summarise(sd(conf_int))

t.test(df %>% filter(wide_range == 0) %>% select(conf_int), 
       df %>% filter(wide_range == 1) %>% select(conf_int), 
       alternative = "two.sided")

##### Narrow ranges ############################################################

df %>% filter(wide_range == 0) %>% select(conf_int) %>% count()
df %>% filter(wide_range == 0) %>% select(conf_int) %>% summarise(mean(conf_int))
df %>% filter(wide_range == 0) %>% select(conf_int) %>% summarise(sd(conf_int))

##### Wide ranges ##############################################################

df %>% filter(wide_range == 1) %>% select(conf_int) %>% count()
df %>% filter(wide_range == 1) %>% select(conf_int) %>% summarise(mean(conf_int))
df %>% filter(wide_range == 1) %>% select(conf_int) %>% summarise(sd(conf_int))

################################################################################
##### Figure 1 Panel B #########################################################
################################################################################

ggplot(data = df,
       aes(fill = as.factor(wide_range), x = range_pos*100)) + 
  geom_histogram(position = "stack",
                 binwidth = 25) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", x = -10, y = 80, label = "Lower bound", angle = 90, size = 3, hjust = "right") +
  geom_vline(xintercept = 50) +
  annotate(geom = "text", x = 40, y = 80, label = "Midpoint", angle = 90, size = 3, hjust = "right") +
  
  geom_vline(xintercept = 100) +
  annotate(geom = "text", x = 90, y = 80, label = "Upper bound", angle = 90, size = 3, hjust = "right") +
  
  scale_x_continuous(breaks = seq(-200, 300, 25),
                     limits = c(-200,300)) +
  xlab("Normalized actual earnings estimates") +
  ylab("Number of observations") +
  scale_fill_manual(values = c("cadetblue4", "bisque4"),
                    labels = c("Narrow range", "Wide range"),
                    name = "") +
  coord_cartesian(ylim = c(0, 80), clip = "off") +
  theme_classic()

################################################################################
##### Figure 2 #################################################################
################################################################################

ggplot(data = df,
       aes(fill = as.factor(wide_range), x = conf_int)) + 
  geom_histogram(position = "stack",
                 binwidth = 5,
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 100, 5),
                     limits = c(0,100)) +
  scale_y_continuous(breaks = seq(0,70,10)) +
  xlab("Confidence levels") +
  ylab("Number of observations") +
  scale_fill_manual(values = c("cadetblue4", "bisque4"),
                    labels = c("Narrow range", "Wide range"),
                    name = "") +
  coord_cartesian(ylim = c(0, 70), clip = "off") +
  theme_classic()

################################################################################
##### Online Appendix 4A Panel A ###############################################
################################################################################

reg_pessimism_1 <- feols(data = df,
                         pessimism_1 ~
                           wide_range +
                           age +
                           gender +
                           group +
                           public +
                           sales +
                           employees +
                           topmanager +
                           work_experience +
                           acct_experience +
                           forecast_experience,
                         cluster = "industry",
                         fixef = "industry")

reg_pessimism_2 <- feols(data = df,
                         pessimism_2 ~
                           wide_range +
                           age +
                           gender +
                           group +
                           public +
                           sales +
                           employees +
                           topmanager +
                           acct_experience +
                           work_experience +
                           forecast_experience,
                         cluster = "industry",
                         fixef = "industry")

reg_true_belief <- feols(data = df,
                         true_belief ~
                           wide_range +
                           age +
                           gender +
                           group +
                           public +
                           sales +
                           employees +
                           topmanager +
                           acct_experience +
                           work_experience +
                           forecast_experience,
                         cluster = "industry",
                         fixef = "industry")

reg_conf_int <- feols(data = df,
                      conf_int ~
                        wide_range +
                        age +
                        gender +
                        group +
                        public +
                        sales +
                        employees +
                        topmanager +
                        acct_experience +
                        work_experience +
                        forecast_experience,
                      cluster = "industry",
                      fixef = "industry")

reg_em_accrual <- feols(data = df,
                        em_accrual ~
                          wide_range +
                          age +
                          gender +
                          group +
                          public +
                          sales +
                          employees +
                          topmanager +
                          acct_experience +
                          work_experience +
                          forecast_experience,
                        cluster = "industry",
                        fixef = "industry")

reg_em_real <- feols(data = df,
                     em_real ~
                       wide_range +
                       age +
                       gender +
                       group +
                       public +
                       sales +
                       employees +
                       topmanager +
                       acct_experience +
                       work_experience +
                       forecast_experience,
                     cluster = "industry",
                     fixef = "industry")

oa_4a_panel_a <- modelsummary(
  list(favorability = reg_pessimism_1,
       true_belief = reg_pessimism_2,
       expectations = reg_true_belief,
       confidence = reg_conf_int,
       em_accrual = reg_em_accrual,
       em_real = reg_em_real),
  stars = T)

oa_4a_panel_a

################################################################################
##### Online Appendix 4A Panel B ###############################################
################################################################################

reg_inf_range <- feols(data = df,
                       inf_range ~
                         wide_range +
                         age +
                         gender +
                         group +
                         public +
                         sales +
                         employees +
                         topmanager +
                         acct_experience +
                         work_experience +
                         forecast_experience,
                       cluster = "industry",
                       fixef = "industry")

reg_inf_point <- feols(data = df,
                       inf_point ~
                         wide_range +
                         age +
                         gender +
                         group +
                         public +
                         sales +
                         employees +
                         topmanager +
                         acct_experience +
                         work_experience +
                         forecast_experience,
                       cluster = "industry",
                       fixef = "industry")

reg_inf_mid <- feols(data = df,
                     inf_mid ~
                       wide_range +
                       age +
                       gender +
                       group +
                       public +
                       sales +
                       employees +
                       topmanager +
                       acct_experience +
                       work_experience +
                       forecast_experience,
                     cluster = "industry",
                     fixef = "industry")

reg_inf_open <- feols(data = df,
                      inf_open ~
                        wide_range +
                        age +
                        gender +
                        group +
                        public +
                        sales +
                        employees +
                        topmanager +
                        acct_experience +
                        work_experience +
                        forecast_experience,
                      cluster = "industry",
                      fixef = "industry")

oa_4a_panel_b <- modelsummary(
  list(point = reg_inf_point,
       range = reg_inf_range,
       hybrid = reg_inf_mid,
       open = reg_inf_open),
  stars = T)

oa_4a_panel_b

################################################################################
##### Online Appendix 4B Panel A ###############################################
################################################################################

group <- tidy(t.test(data = df, group ~ late_responder), conf.int = T) %>% 
  mutate(name = "group")
public <- tidy(t.test(data = df, public ~ late_responder), conf.int = T) %>% 
  mutate(name = "public")
sales <- tidy(t.test(data = df, sales ~ late_responder), conf.int = T) %>% 
  mutate(name = "sales")
employees <- tidy(t.test(data = df, employees ~ late_responder), conf.int = T) %>% 
  mutate(name = "employees")
work_experience <- tidy(t.test(data = df, work_experience ~ late_responder), conf.int = T) %>% 
  mutate(name = "work_experience")
position <- tidy(t.test(data = df, topmanager ~ late_responder), conf.int = T) %>% 
  mutate(name = "topmanager")
acct_experience <- tidy(t.test(data = df, acct_experience ~ late_responder), conf.int = T) %>% 
  mutate(name = "acct_experience")
forecast_experience <- tidy(t.test(data = df, forecast_experience ~ late_responder), conf.int = T) %>% 
  mutate(name = "forecast_experience")
age <- tidy(t.test(data = df, age ~ late_responder), conf.int = T) %>% 
  mutate(name = "age")
gender <- tidy(t.test(data = df, gender ~ late_responder), conf.int = T) %>% 
  mutate(name = "gender")

oa_4b_panel_a <- rbind(group,
                       public,
                       sales,
                       employees,
                       work_experience,
                       position,
                       acct_experience,
                       forecast_experience,
                       age,
                       gender)

nice_table(oa_4b_panel_a, broom = "t.test")

################################################################################
##### Online Appendix 4B Panel B ###############################################
################################################################################

favorability <- tidy(t.test(data = df, pessimism_1 ~ late_responder), conf.int = T) %>% 
  mutate(name = "favorability")
true_belief <- tidy(t.test(data = df, pessimism_2 ~ late_responder), conf.int = T) %>%
  mutate(name = "true_belief")
expectations <- tidy(t.test(data = df, true_belief ~ late_responder), conf.int = T) %>%
  mutate(name = "expectations")
confidence <- tidy(t.test(data = df, conf_int ~ late_responder), conf.int = T) %>%
  mutate(name = "confidence")
em_accrual <- tidy(t.test(data = df, em_accrual ~ late_responder), conf.int = T) %>%
  mutate(name = "em_accrual")
em_real <- tidy(t.test(data = df, em_real ~ late_responder), conf.int = T) %>%
  mutate(name = "em_real")
point <- tidy(t.test(data = df, inf_point ~ late_responder), conf.int = T) %>%
  mutate(name = "inf_point")
range <- tidy(t.test(data = df, inf_range ~ late_responder), conf.int = T) %>%
  mutate(name = "range")
hybrid <- tidy(t.test(data = df, inf_mid ~ late_responder), conf.int = T) %>%
  mutate(name = "hybrid")
open <- tidy(t.test(data = df, inf_open ~ late_responder), conf.int = T) %>%
  mutate(name = "open")

oa_4b_panel_b <- rbind(favorability,
                       true_belief,
                       expectations,
                       confidence,
                       em_accrual,
                       em_real,
                       point,
                       range,
                       hybrid,
                       open)

nice_table(oa_4b_panel_b, broom = "t.test")

################################################################################
##### Online Appendix 5 ########################################################
################################################################################

oa_5 <- df %>%
  select(inf_range, inf_mid, inf_point, inf_open) %>%
  pivot_longer(cols = c(inf_range, inf_mid, inf_point, inf_open))

oa_5$name <- factor(oa_5$name, levels=c("inf_point", "inf_range", "inf_mid", "inf_open"),
                    labels = c("Point", "Range", "Hybrid", "Open"))

ggplot(oa_5) +
  geom_bar(aes(x = value, fill = as.factor(name)), position = "dodge", color = "black") +
  scale_fill_manual(values = c("grey", "bisque4", "cadetblue4", "darkslategrey")) +
  xlab("Informativeness Rating") +
  ylab("Number of observations") +
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(n.breaks = 8) +
  guides(fill=guide_legend(title="Type")) +
  theme_classic()

################################################################################
##### Online Appendix 6 ########################################################
################################################################################

ggplot(data = df %>% 
         select(em_real, em_accrual) %>% 
         pivot_longer(cols=c('em_accrual', 'em_real'), 
                      names_to='Type', 
                      values_to="Likelihood")) +
  geom_bar(mapping = aes(x = Likelihood, fill = Type),
           position = "dodge",
           color = "black") +
  scale_y_continuous(breaks = (0:9)*10,
                     name = "Number of observations") +
  scale_x_continuous(breaks = 1:7) +
  scale_fill_manual(values = c("cadetblue4", "bisque4"),
                    labels = c("Accrual earnings management", "Real earnings management")) +
  theme_classic()