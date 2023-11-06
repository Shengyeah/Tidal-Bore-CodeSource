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
##
## HERE COMES ANALYSE TWO: GLOBALIZATION VS. ANTI-GLOBALIZATION
dta_1 <- read.csv("./按照table2分析/remote/True.csv") # import globalization data
dta_plt3 <- dta_1 %>% 
  group_by(year, project_Themename) %>% 
  summarise(fund_true = mean(project_alreadyfunding, na.rm = TRUE), 
            nmb_true = mean(numberofDonations, na.rm = TRUE)) #  group numbers of donations persons and funding per person by year and project themes
dta_plt3_tmp <- dta_1 %>% 
  group_by(year) %>% 
  summarise(fund_true = mean(project_alreadyfunding, na.rm = TRUE), 
            nmb_true = mean(numberofDonations, na.rm = TRUE))#  group numbers of donations persons and funding per person only by year

dta_2 <- read.csv("./按照table2分析/remote/False.csv")# import anti-globalization data
dta_plt3_01 <- dta_2 %>% 
  group_by(year, project_Themename) %>% 
  summarise(fund_false = mean(project_alreadyfunding, na.rm = TRUE), 
            nmb_false = sum(numberofDonations, na.rm = TRUE))#  group numbers of donations persons and funding per person by year and project themes
dta_plt3_tmp1 <- dta_2 %>% 
  group_by(year) %>% 
  summarise(fund_false = mean(project_alreadyfunding, na.rm = TRUE), 
            nmb_false = mean(numberofDonations, na.rm = TRUE))#  group numbers of donations persons and funding per person only by year

dta_plt3 <- left_join(dta_plt3_01, dta_plt3, by = c("year", "project_Themename")) # combine globalization data and anti-globalization data with year and project themes
dta_plt3_tmp <- left_join(dta_plt3_tmp1, dta_plt3_tmp, by = "year")# combine globalization data and anti-globalization data with only year
dta_plt3$nmb_true[is.na(dta_plt3$nmb_true)] <- 0 # replace missing value in number of donations as zero 
dta_plt3$fund_true[is.na(dta_plt3$fund_true)] <- 0 # replace missing value in funding as zero
dta_plt3 <- na.omit(dta_plt3) # remove missing rows


dta_plt3 <- dta_plt3 %>% 
  mutate(fund = fund_false + fund_true, remote_rate1 = fund_true / (fund_true + fund_false), 
         nmb = nmb_false + nmb_true, remote_rate2 = nmb_true / (nmb_true + nmb_false)) # add funded total amount, total numbers of donations, and their remote rate

dta_plt3_tmp <- dta_plt3_tmp %>% 
  mutate(fund = fund_false + fund_true, remote_rate1 = fund_true / (fund_true + fund_false), 
         nmb = nmb_false + nmb_true, remote_rate2 = nmb_true / (nmb_true + nmb_false))# add funded total amount, total numbers of donations, and their remote rate in year data

dta_plt3 <- dta_plt3[dta_plt3$year != 2023, ] # remove data in year 2023
dta_plt3_tmp <- dta_plt3_tmp[dta_plt3_tmp$year != 2023, ] # remove data in year 2023
ggplot(data = dta_plt3_tmp, aes(x = year)) + 
  geom_point(aes(y = remote_rate1), shape = 16, size = 5, alpha = 0.7, color = "aquamarine4")  + 
  geom_smooth(aes(y = remote_rate1), color = "aquamarine4", se = FALSE,linetype = "dashed")+ 
  geom_point(aes(y = remote_rate2), alpha = 0.7, size = 5, shape = 17, color = "coral2")+ 
  geom_smooth(aes(y = remote_rate2), color = "coral2", se = FALSE,linetype = "dashed") + 
  theme_test() # create point and smooth line map with number of donation and funded remote rate

# create vectors of three types of globalization 
globalization <- c("Arts and Culture", "Animal Welfare", "Disability Rights", "Refugee Rights", "Reproductive Health")
deglobal <- c("Justice and Human Rights","Peace and Reconciliation", "Safe Housing",  "Ending Human Trafficking", "Sport")
apglobal <- c("Gender Equality", "Education", "Economic Growth", "Disaster Response", "Food Security")

# generate a new column to cover project themes into three different globalization type
dta_plt3$type <- rep("0", nrow(dta_plt3)) 
dta_plt3$type[dta_plt3$project_Themename %in% globalization] <- "global"
dta_plt3$type[dta_plt3$project_Themename %in% deglobal] <- "anti-global"
dta_plt3$type[dta_plt3$project_Themename %in% apglobal] <- "partial-global"

# generate a new column to Assist in generating group diagrams
plt1 <- c("Arts and Culture", "Justice and Human Rights","Gender Equality")
plt2 <- c("Animal Welfare","Peace and Reconciliation","Education")
plt3 <- c("Disability Rights","Safe Housing","Economic Growth")
plt4 <- c("Refugee Rights", "Ending Human Trafficking","Disaster Response")
plt5 <- c("Reproductive Health","Sport","Food Security")

# cover the project themes into five assistant variables
dta_plt3$type1 <- rep("0", nrow(dta_plt3))
dta_plt3$type1[dta_plt3$project_Themename %in% plt1] <- "1"
dta_plt3$type1[dta_plt3$project_Themename %in% plt2] <- "2"
dta_plt3$type1[dta_plt3$project_Themename %in% plt3] <- "3"
dta_plt3$type1[dta_plt3$project_Themename %in% plt4] <- "4"
dta_plt3$type1[dta_plt3$project_Themename %in% plt5] <- "5"


dta_plt3 %>% filter(type != "0", type1 != "0") %>% # remove the observations that do not belong to a given type
ggplot(aes(x = year)) + 
  geom_point(aes(y = remote_rate1), shape = 16, size = 5, alpha = 0.7, color = "aquamarine4")  + 
  geom_smooth(aes(y = remote_rate1), color = "aquamarine4", se = FALSE,linetype = "dashed")+ 
  geom_point(aes(y = remote_rate2), alpha = 0.7, size = 5, shape = 17, color = "coral2")+ 
  geom_smooth(aes(y = remote_rate2), color = "coral2", se = FALSE,linetype = "dashed") + 
  facet_grid(type1 ~ type) + 
  theme_test() # Generate a composite graph of different project themes remote ratios over time
