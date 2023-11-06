# import package
library(tidyverse)
library(openxlsx)
library(strucchange)
library(reshape2)
library(ggrepel)
library(strucchange)

# set work dir
setwd("D:/2023年_工作/UN Datathon")

# import total data
dta <- read.csv("table1非活跃项目总表1105.csv")
dta <- dta[dta$if_active == "False",] # clean out onging projects
dta <- dta[dta$year != 2023, ] # clean out projects in 2023

dta <- dta %>%  
  mutate(funed_rate = project_alreadyfunding/project_goal,
         avg_perfund = project_alreadyfunding/numberofDonations) # select data and mutate funding complete rate, funding per person of donation
dta <- dta[scale(dta$funed_rate) <= 3,] # remove abnormal value
dta$avg_perfund[dta$avg_perfund == Inf] <- NA # remove infinity value


##
##
## HERE COMES ANALYSE FOUR: SURVIAL ANALYSIS
# import packages
library(survival)
library(survminer)

dta_plt8 <- dta %>% 
  mutate(funed_rate = project_alreadyfunding/project_goal) %>%  #mutate the funded rate column using the already funding compare to the goal
  filter(interval_day > 0, !is.na(interval_day), !is.na(country_group), !is.na(organization_country_group), organization_country_group != "", country_group!="") #remove the missing value rows

dta_plt8$funed_rate_type = dta_plt8$funed_rate > 0.8 # using the 80% completed rate as survival
dta_plt8$funed_rate_type <- as.numeric(as.factor(dta_plt8$funed_rate_type)) #change the survival column as numerical class
dta_plt8$country_group <- factor(dta_plt8$country_group, level = c("低收入国家", "中等收入国家", "高收入国家")) # change the country group as factor class
dta_plt8$organization_country_group <- factor(dta_plt8$organization_country_group, levels = c("低收入国家", "中等收入国家", "高收入国家")) #change the organization country group as factor class

fit <- survfit(Surv(interval_month, funed_rate_type) ~ country_group, data = dta_plt8)# fit the survival model using the target areas groups

ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # adding risk table
           risk.table.col = "strata", # change the risk table color using the strata
           linetype = "strata", # change the line type using the strata
           surv.median.line = "hv", # showing the vertical and horizontal reference lines
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF", "coral"))


fit <- survfit(Surv(interval_month, funed_rate_type) ~ organization_country_group, data = dta_plt8) # fit the survival model using the initiative areas groups
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # adding risk table
           risk.table.col = "strata", # change the risk table color using the strata
           linetype = "strata", # change the line type using the strata
           surv.median.line = "hv", # showing the vertical and horizontal reference lines
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF", "coral"))


# generate a new column to cover project themes into three different globalization type
dta_plt8$pj_theme_type <- rep("0", nrow(dta_plt8))
dta_plt8$pj_theme_type[dta_plt8$project_Themename %in% globalization] <- "global"
dta_plt8$pj_theme_type[dta_plt8$project_Themename %in% deglobal] <- "anti-global"
dta_plt8$pj_theme_type[dta_plt8$project_Themename %in% apglobal] <- "partial-global"

#filter the project theme in the three types of globalization
fit_new <- dta_plt8 %>% filter(pj_theme_type %in% c("global", "anti-global", "partial-global")) 
fit_new$pj_theme_type <- factor(fit_new$pj_theme_type, levels = c("global", "partial-global", "anti-global"))

fit <- survfit(Surv(interval_day, funed_rate) ~ as.factor(pj_theme_type), data = fit_new)
# fit the survival model using the initiative areas groups
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # adding risk table
           risk.table.col = "strata", # change the risk table color using the strata
           linetype = "strata", # change the line type using the strata
           surv.median.line = "hv", # showing the vertical and horizontal reference lines
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF", "coral"))

