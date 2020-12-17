
library(tidyverse)
library(kableExtra)
library(gtsummary)
library(scales)
library(data.table)
library(dplyr)

# 1. load_data----------
main_data = data.table::fread(
  "Z:/George_Surgeon_Projects/non_certified_surgeons_char/medicare_ABS.csv"
) 

###remove unnecessary variables in main_data
slim_main_data = main_data %>%
    select(flg_male,
           id_physician_npi,
           us_medschool.x,
           CE_pass,
           linked_abs,
           nppes_gs_specialty) %>%
    filter(linked_abs == "linked_abs")
    

###Load practice pattern 20% analytic file from practice patterns project
medicare_practice_pattern = data.table::fread("Z://George_Surgeon_Projects/medicare20pct/analytic_practice_pattern.csv")

##NPI as factor for merging
medicare_practice_pattern$id_physician_npi <- as.factor(medicare_practice_pattern$id_physician_npi)
slim_main_data$id_physician_npi <- as.factor(slim_main_data$id_physician_npi)
data$id_physician_npi <- as.factor(data$id_physician_npi)
data$member_id <- as.factor(data$member_id)
medicare_practice_pattern$member_id <- as.factor(medicare_practice_pattern$member_id)

#filter unique NPIs

unique_slim <- slim_main_data[!duplicated(slim_main_data$id_physician_npi)]

#inner_join tables for ABS linked noncert surgeons and practice patterns

combined_data<- merge(unique_slim, medicare_practice_pattern, by = c("id_physician_npi"))
  
# define binary emergent admission- case level

main_data = combined_data%>% 
  mutate(emergency_admit = case_when(e_admit_type %in% c("1_Emergency", "2_Urgent") ~ "emergent",
                                     e_admit_type %in% c("3_Elective", "4_Other") ~ "elective"),
         race = case_when(e_race_wbho == 1 ~ "white",
                          e_race_wbho == 2 ~ "black",
                          e_race_wbho == 3 ~ "hispanic",
                          e_race_wbho == 4 ~ "other"),
         certified = factor(CE_pass, levels = c(0,1),
                          labels = c("non-cert", "cert")),
         sex = ifelse(flg_male=="1" | flg_male=="m", flg_male, NA))


#Check table
main_data %>%
  group_by(certified) %>%
  summarise(n_cases = n(),
            n_surg = length(unique(id_physician_npi)),
            cases_per_surg = (n_cases / n_surg))


# case (patient) level ----
###### Patient SES Quintile 1=1st quintile (poorest), 2=2nd quintile, 3=3rd quintile, 4=4th quintile, 5=5th (quintile)(least poor)

case_level = combined_data %>% 
  select(
    certified,
    flg_male,
    race,
    e_ses_5grp,
    emergency_admit,
    e_uic_2013,
    val_yr_practice.x,
    e_npi_urban_3grp,
    flg_npi_teaching,
    flg_hosp_region_ne,
    flg_hosp_region_west,
    flg_hosp_region_midwest,
    flg_hosp_region_south,
    val_npi_avgvol_12to16
    ) 

case_tbl = case_level %>% 
  tbl_summary(by = certified, missing="no") %>% 
  add_p() %>% 
  bold_labels()

main_data %>% 
  group_by(certified, e_proc_grp_lbl) %>% 
  summarise(n_case = n(), 
            n_surgeon = length(unique(id_physician_npi)),
            case_per_surgeon = n_case / n_surgeon)
  
# surgeon level descriptive

surg_level = main_data %>%
  distinct(certified,
           id_physician_npi,
           sex,
           us_medschool.x,
           CE_pass,
           ) 

surg_tbl = surg_level %>% 
  select(-id_physician_npi) %>% 
  tbl_summary(by = certified, missing = "no") %>%
  add_p() %>% 
  bold_labels()


# comparing descriptive per surgeon 
main_data %>%
  filter(emergency_admit == "emergent") %>%
  group_by(certified) %>%
  summarise(emergent_case_per_surgeon = (n_cases = n(), n_surg = length(unique(id_physician_npi)), mean_case = n_cases/n_surg)) %>%
  kable(caption = "Descriptives Per Surgeon") %>% 
  kable_styling(full_width = F) 
       






# Involvement of additional surgeons
#***Surgeon level:
#Compare rate of cases worked with multiple surgeons in Certified and non certified groups
#0 = solo/isolated practice, 1 = worked with another surgeon at least once

main_data %>%
  group_by(id_physician_npi) %>%
  filter(sum(flg_multi_surgeon) == 0 ) %>%
  group_by(certified) %>%
  summarise(n_solo_surg = length(unique(id_physician_npi))) %>%
  kable(caption = "number of solo practicing surgeons by certification") %>% 
  kable_styling(full_width = F) 



# Urban vs. Rural practice
# level: Medicare flag (20pct file) E_NPI_URBAN_3GRP,"Surgeon urban/rural status: 1-Urban (>80% cases operated in urban), 2- Mixed (>20% & <=80% cases done in urban), 3- Rural (<=20% cases done in urban)"
# Utilize E_UIC_2013, Urban influence code from 2013, utilize previous matching of hospital IDs, Code 1+2 = Urban, 3-12 = Rural

main_data %>%
  distinct(id_physician_npi, e_npi_urban_3grp, certified) %>%
  group_by(certified) %>%
  count(e_npi_urban_3grp)
 




### Distribution of Cases
main_data %>% 
  group_by(certified) %>%
  select(e_ecs_lbl, certified) %>% 
  tbl_summary(by = certified,missing = "no") 







##### Hospital region
#Case level: Identify file flag "E_HOSP_REGION_4GRP" Hosp: Region 1=North-East, 2=West, 3=Mid-west, 4=South
#Report distribution as percent 






