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
## HERE COMES ANALYSE FIVE: CORRESPONDENCE ANALYSIS

#set random seed
set.seed(1234)

#import package
library(circlize)

dta_plt9 <- dta %>% 
  filter(project_Themename %in% c(globalization, deglobal, apglobal)) %>%  #filter the project theme in the three types of globalization
  group_by(project_Themename, country_group) %>% 
  count() %>% 
  filter(country_group != "") %>% #remove the empty rows
  dcast(country_group ~ project_Themename) #reshape the data using the country group as row and project theme as column
dta_plt9 <- dta_plt9[c(1,3,2),] #re-order the data
mat <- as.matrix(dta_plt9[2:length(dta_plt9)]) #create the Contingency Table
row.names(mat) <- dta_plt9[[1]] #rename the row names of Contingency Table
chordDiagram(mat, grid.col = c(低收入国家 = "#E7B800", 中等收入国家 = "#2E9FDF", 高收入国家 = "coral")) #create the chord Diagram of Contingency Table

#import package
library(ca)

dta_plt9 <- dta %>% 
  filter(project_Themename %in% c(globalization, deglobal, apglobal), year <= 2008) %>% # filter data within year 2008 
  group_by(project_Themename, organization_country_group) %>% 
  count() %>% 
  filter(organization_country_group != "") %>% #remove the empty rows
  dcast(organization_country_group ~ project_Themename) #reshape the data using the country group as row and project theme as column
dta_plt9[is.na(dta_plt9)] <- 0 
mat <- as.matrix(dta_plt9[2:length(dta_plt9)]) #create the Contingency Table
row.names(mat) <- dta_plt9[[1]] #rename the row names of Contingency Table
plot(ca(mat), mass = TRUE, contrib = 'absolute', map = 'rowgreen', arrows = c(FALSE, TRUE)) # create the correspondence analysis map

dta_plt9 <- dta %>% 
  filter(project_Themename %in% c(globalization, deglobal, apglobal), year > 2008, year <= 2015) %>% # filter data within year 2008 to 2015 
  group_by(project_Themename, organization_country_group) %>% 
  count() %>% 
  filter(organization_country_group != "") %>% #remove the empty rows
  dcast(organization_country_group ~ project_Themename) #reshape the data using the country group as row and project theme as column
dta_plt9[is.na(dta_plt9)] <- 0 
mat <- as.matrix(dta_plt9[2:length(dta_plt9)]) #create the Contingency Table
row.names(mat) <- dta_plt9[[1]] #rename the row names of Contingency Table
plot(ca(mat), mass = TRUE, contrib = 'absolute', map = 'rowgreen', arrows = c(FALSE, TRUE)) # create the correspondence analysis map

dta_plt9 <- dta %>% 
  filter(project_Themename %in% c(globalization, deglobal, apglobal), year > 2015, year <= 2022) %>% # filter data within year 2015 to 2022 
  group_by(project_Themename, organization_country_group) %>% 
  count() %>% 
  filter(organization_country_group != "") %>% #remove the empty rows
  dcast(organization_country_group ~ project_Themename) #reshape the data using the country group as row and project theme as column
dta_plt9[is.na(dta_plt9)] <- 0 
mat <- as.matrix(dta_plt9[2:length(dta_plt9)]) #create the Contingency Table
row.names(mat) <- dta_plt9[[1]] #rename the row names of Contingency Table
plot(ca(mat), mass = TRUE, contrib = 'absolute', map = 'rowgreen', arrows = c(FALSE, TRUE)) # create the correspondence analysis map
