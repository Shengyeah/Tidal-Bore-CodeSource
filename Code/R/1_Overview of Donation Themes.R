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
## HERE COMES ANALYSE ONE: AN OVERVIEW OF DONATION THEMES
dta_plt1 <- dta %>% group_by(project_Themename) %>%
  summarise(funed_rate = mean(funed_rate, na.rm = TRUE), 
            avg_perfund = mean(avg_perfund, na.rm = TRUE),
            numberofDonations = mean(numberofDonations, na.rm = TRUE)) # group funding complete rate, funding per person of donation by project theme
dta_plt1 <- na.omit(dta_plt1) #remove missing rows
dta_plt1 <- dta_plt1[-1, ] #remove empty rows

ggplot(data = dta_plt1, aes(log(numberofDonations), log(avg_perfund))) + 
  geom_point(alpha = 0.7, aes(size = sqrt(funed_rate), color = funed_rate)) + 
  geom_text_repel(aes(label = project_Themename), size = 5)+ theme_bw() + 
  scale_color_gradient(low = "ghostwhite", high = "coral2") + 
  geom_hline(aes(yintercept = mean(log(avg_perfund)))) + 
  geom_vline(aes(xintercept = mean(log(numberofDonations)))) +
  scale_size_continuous(range=c(1, 25)) #visualization overview map

