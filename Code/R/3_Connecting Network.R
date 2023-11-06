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
## HERE COMES ANALYSE THREE: WORLD CONNECTING MAPPING

# import packages
library(igraph)
library(ggraph)
library(ggpubr)
library(rstatix)
library(ggspatial)
library(ggrepel)
library(sf)
library(gganimate)
library(gifski)
library(av)

dta_1 <- read.csv("./按照table2分析/remote/True.csv") # import globalization data
dta_1 <- dta_1[dta_1$year > 2015 & dta_1$year <= 2022, ]


# read world map data
json_data <- read_sf("world.json")





##
##connecting overview map
dta_plt4 <- dta_1 %>% 
  group_by(country_latitude, country_longitude, organization_country_latitude, organization_country_longitude) %>% 
  count() # generate initiative areas and target areas with project items count
dta_plt4 <- na.omit(dta_plt4) # remove the missing value rows

ggplot() + geom_sf(data = json_data, fill = NA) + # using the json map data as underlay
  geom_curve(data = dta_plt4[dta_plt4$n > 1,], aes(x = organization_country_longitude, y = organization_country_latitude, xend = country_longitude, yend = country_latitude), alpha = 0.5, arrow = arrow(type = "closed", length=unit(0.20,"cm"))) +
  coord_sf() + theme_test() # create the map with initiative areas to target areas


##
##connecting map with degree filling
dta_plt5 <- dta_1 %>% 
  group_by(organization_isocode, country_code) %>% 
  count() #create the initiative and target areas project items count data  
dta_plt5_01 <- dta_plt5 %>% 
  group_by(organization_isocode) %>% 
  summarise(n_from = sum(n, na.rm = TRUE)) # integrate the initiative data
names(dta_plt5_01)[1] <- "country_code" # rename the first column as country_code
dta_plt5_02 <- dta_plt5 %>% 
  group_by(country_code) %>% 
  summarise(n_to = sum(n, na.rm = TRUE)) # integrate the target data
dta_plt5 <- full_join(dta_plt5_01, dta_plt5_02, by = "country_code") # full combine the target and the initiative data
dta_plt5$n_from[is.na(dta_plt5$n_from)] <- 0 # replace the missing value as zero
dta_plt5$n_to[is.na(dta_plt5$n_to)] <- 0 # replace the missing value as zero

names(dta_plt5)[1] <- "id" # rename the first column as id
json_data <- left_join(json_data, dta_plt5, by = "id") #combine the json map data and the combined data by id

ggplot() + geom_sf(data = json_data, aes(fill = log(n_from))) + #using the json map filled with initiative data as underlay
  geom_curve(data = dta_plt4[dta_plt4$n > 1,], aes(x = organization_country_longitude, y = organization_country_latitude, xend = country_longitude, yend = country_latitude), alpha = 0.5, arrow = arrow(type = "closed", length=unit(0.20,"cm"))) +
  coord_sf() + # create the map with initiative areas to target areas
  scale_fill_gradient(low = "ghostwhite", high = "aquamarine4", na.value = NA) +theme_test() #theme color change

ggplot() + geom_sf(data = json_data, aes(fill = log(n_to))) + #using the json map filled with target data as underlay
  geom_curve(data = dta_plt4[dta_plt4$n > 1,], aes(x = organization_country_longitude, y = organization_country_latitude, xend = country_longitude, yend = country_latitude), alpha = 0.5, arrow = arrow(type = "closed", length=unit(0.20,"cm"))) +
  coord_sf() + # create the map with initiative areas to target areas
  scale_fill_gradient(low = "ghostwhite", high = "coral2", na.value = NA) +theme_test() #theme color change




##
## animation map
dta_plt4_in <- dta %>% 
  group_by(year, country_code) %>% 
  summarise(n = mean(project_alreadyfunding, na.rm = TRUE)) # generate initiative areas donations funded
names(dta_plt4_in)[2] <- "id" # rename the second column as "id"
dta_plt4_out <- dta_1 %>% 
  group_by(year, organization_isocode) %>% 
  summarise(n = mean(project_alreadyfunding, na.rm = TRUE)) # generate target areas donations funded
names(dta_plt4_out)[2] <- "id" # rename the second column as id
dta_plt4 <- full_join(dta_plt4_out, dta_plt4_in, by = c("year", "id")) # combine the initiative and target areas data
dta_plt4$n.x[is.na(dta_plt4$n.x)] <- 0 # replace the missing value in initiative as zero
dta_plt4$n.y[is.na(dta_plt4$n.y)] <- 0 # replace the missing value in target as zero

dta_plt4 <- dta_plt4 %>% filter(n.x > 2 | n.y > 60000) #filter the initiative and target areas

i <- 1
# using while loop to 5th order smoothing the initiative and target areas donation amount
while (i <= 5) {
  dta_plt4_ad1 <- dta_plt4
  dta_plt4_ad1$year <- dta_plt4_ad1$year + i
  dta_plt4 <-full_join(dta_plt4, dta_plt4_ad1, by = c("id", "year"))
  dta_plt4[3:6][is.na(dta_plt4[3:6])] <- 0
  dta_plt4 <- mutate(dta_plt4, n.x = (n.x.x + n.x.y)/2, n.y = (n.y.x + n.y.y)/2)
  dta_plt4 <- select(dta_plt4, id, year, n.x, n.y)
  i <- i + 1
}


dta_plt4_ct <- dta_1 %>% 
  group_by(country_code) %>% 
  summarise(type = first(country_group), gdp = mean(country_gdp, na.rm = TRUE)) # generate the areas id, GDP and group
names(dta_plt4_ct)[1] <- "id" # rename the first column as id
dta_plt4 <- left_join(dta_plt4, dta_plt4_ct, by = "id") #combine the areas data and donation amount data


dta_plt4$year <- as.integer(dta_plt4$year) #transfer the year column as integer class
dta_plt4 <- dta_plt4[dta_plt4$type != "" & !is.na(dta_plt4$type),] #filter the type
dta_plt4 <- dta_plt4[dta_plt4$year <= 2018, ] # filter the year within 2018


dta_plt4$type <- factor(dta_plt4$type, levels = c("低收入国家", "中等收入国家", "高收入国家")) #transfer the type from character to factor class

p <- ggplot(data = dta_plt4, aes(log(n.x + 1), log(n.y + 1))) + 
  geom_point(aes(color = type, size = gdp), alpha = 0.5) +
  geom_text(aes(label = id), size = 20) +
  labs(title = 'Year: {frame_time}') +
  scale_x_continuous(limits = c(-2, 8))+
 # facet_grid(. ~ type) +
  scale_size_continuous(range=c(1, 40)) +
  transition_time(year) + 
  theme_bw() +
  ease_aes('linear') # create the animation figure with in and out degree
#save the animation map
anim_save(p, filename = "gif.mp4", fps = 30, duration = 20, device = 'png', width = 2000, height = 1800)




##
##Geographic center changing map
dta$numberofDonations[dta$numberofDonations == 0] <- NA # replace the missing value in numbers of donations as zero
sm_avgfund <- sum(dta$project_alreadyfunding / dta$numberofDonations, na.rm = TRUE) # calculate the total amount of average donations per person
sm_nmb <- sum(dta$numberofDonations, na.rm = TRUE) # calculate the total amount of numbers of donations



dta_plt6 <- dta %>%
  filter(project_Themename %in% apglobal)  %>% 
  group_by(year) %>% 
  summarise(
  from_avgfund_lati = sum(organization_country_latitude * (project_alreadyfunding / numberofDonations), na.rm = TRUE) / sm_avgfund,
  from_avgfund_longi = sum(organization_country_longitude * (project_alreadyfunding / numberofDonations), na.rm = TRUE) / sm_avgfund,
  from_nmb_lati = sum(organization_country_latitude * numberofDonations, na.rm = TRUE) / sm_nmb,
  from_nmb_longi = sum(organization_country_longitude * numberofDonations, na.rm = TRUE) / sm_nmb, #calculate the geographical center of initiative areas with total amount of donations and persons
  to_avgfund_lati = sum(organization_country_latitude * (project_alreadyfunding / numberofDonations), na.rm = TRUE) / sm_avgfund,
  to_avgfund_longi = sum(country_longitude * (project_alreadyfunding / numberofDonations), na.rm = TRUE) / sm_avgfund,
  to_nmb_lati = sum(country_latitude * numberofDonations, na.rm = TRUE) / sm_nmb,
  to_nmb_longi = sum(country_longitude * numberofDonations, na.rm = TRUE) / sm_nmb #calculate the geographical center of target areas with total amount of donations and persons
)

dta_plt6 <- na.omit(dta_plt6) # remove the missing value rows
dta_plt6 <- dta_plt6[dta_plt6$year != 2023,] # remove the data in year 2023

ggplot() + geom_sf(data = json_data, fill = NA) + # using the json map data with no fill as underlay
  geom_path(data = dta_plt6, aes(x = from_avgfund_longi, y = from_avgfund_lati), color = "coral2", arrow = arrow(type = "closed", length=unit(0.20,"cm"))) + #mapping the geographical center of initiative areas using donations
  geom_path(data = dta_plt6, aes(x = from_nmb_longi, y = from_nmb_lati), color = "coral2", arrow = arrow(type = "closed", length=unit(0.20,"cm")), linetype="dashed") + #mapping the geographical center of initiative areas using persons
  geom_path(data = dta_plt6, aes(x = to_avgfund_longi, y = to_avgfund_lati), color = "aquamarine4", arrow = arrow(type = "closed", length=unit(0.20,"cm"))) + #mapping the geographical center of target areas using donations
  geom_path(data = dta_plt6, aes(x = to_nmb_longi, y = to_nmb_lati), color = "aquamarine4", arrow = arrow(type = "closed", length=unit(0.20,"cm")), linetype="dashed") + #mapping the geographical center of target areas using persons
  coord_sf() + theme_test()


ggplot() +
  geom_path(data = dta_plt6, aes(x = from_avgfund_longi, y = from_avgfund_lati), color = "coral2", arrow = arrow(type = "closed", length=unit(0.20,"cm")))  + # zoom the the geographical center of initiative areas using donations
  geom_path(data = dta_plt6, aes(x = to_avgfund_longi, y = to_avgfund_lati), color = "aquamarine4", arrow = arrow(type = "closed", length=unit(0.20,"cm")))  + # zoom the the geographical center of target areas using donations
  
  geom_text(data = dta_plt6, aes(x = from_avgfund_longi, y = from_avgfund_lati, label = year)) + 
  geom_text(data = dta_plt6, aes(x = to_avgfund_longi, y = to_avgfund_lati, label = year)) + 
  coord_sf() + theme_test()

ggplot() +
  geom_path(data = dta_plt6, aes(x = from_nmb_longi, y = from_nmb_lati), color = "coral2", arrow = arrow(type = "closed", length=unit(0.20,"cm")), linetype="dashed") + # zoom the the geographical center of initiative areas using persons
  geom_path(data = dta_plt6, aes(x = to_nmb_longi, y = to_nmb_lati), color = "aquamarine4", arrow = arrow(type = "closed", length=unit(0.20,"cm")), linetype="dashed") + # zoom the the geographical center of target areas using persons
  
  geom_text_repel(data = dta_plt6, aes(x = from_nmb_longi, y = from_nmb_lati, label = year)) +
  geom_text_repel(data = dta_plt6, aes(x = to_nmb_longi, y = to_nmb_lati, label = year)) +
  coord_sf() + theme_test()
