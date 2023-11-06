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


## HERE COMES ANALYSE SIX: REGRESSION ANALYSIS
dta_itu <- read.csv("table2_itu.csv")
dta_itu <- dta_itu %>% 
  select(year, country_code, organization_isocode, country_gdp, organization_country_gdp, or_itu = organization_country_International_bandwidth_per_Internet_user, co_itu = country_International_bandwidth_per_Internet_user, project_alreadyfunding, numberofDonations, country_longitude, country_latitude, organization_country_longitude, organization_country_latitude, project_Themename) %>% 
  filter(year >= 2010, year <= 2020, project_Themename %in% c(apglobal, deglobal, globalization), country_code != organization_isocode) %>% 
  mutate(avg_fund = project_alreadyfunding / numberofDonations)



dta_itu$year_type <- dta_itu$year >= 2016
dta_itu_1 <- dta_itu %>% group_by(year_type, organization_isocode) %>% summarise(country_gdp = mean(country_gdp, na.rm = TRUE), organization_country_gdp = mean(organization_country_gdp, na.rm = TRUE), or_itu = mean(or_itu, na.rm = TRUE), co_itu = mean(co_itu, na.rm = TRUE), dist = mean(dist, na.rm = TRUE), sum_fund = sum(project_alreadyfunding, na.rm = TRUE), nmb = sum(numberofDonations, na.rm = TRUE))


dta_itu_1 %>% ggplot(aes(x = log(co_itu), y = log(sum_fund))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(year_type ~ .)+
  theme_bw()
