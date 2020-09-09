require(tidyverse)
require(magrittr)
require(RColorBrewer)
require(rgdal)
require(ggplot2)
require(lubridate)
require(plotly)
require(reshape2)
require(data.table)
require(RCurl)
require(lodown)
require(srvyr)
require(haven)
require(ipumsr)
require(rlang)
require(scales)
require(naniar)
require(matrixStats)
require(survey)

##Set Working Directory to read the data file. 

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Link to download data is as follows: 
#Onedrive -> City of Seattle -> Innovation + pErformance - Nalaytics -> Projects -> Evan_Projects -> ACS_SeattleFacts
#Make sure you have the two files usa_0009.dat and usa_00009.xml


#We are sourcing data from IPUMS USA. IPUMS harmonizes US ACS data, from 2000 to the present. This data can be accessed at https://usa.ipums.org/usa/index.shtml

#Reading in the data file. Will take a while, so make sure to finish it before runnign anything else!
ddi <- read_ipums_ddi("usa_00009.xml")
data <- read_ipums_micro(ddi)


#Attempt at reading in weights. 
#svy <- svydesign(~CLUSTER, weights = ~PERWT, strata = ~STRATA, data = data, nest = TRUE, check.strata = FALSE)

#Dividing person weights and household weights by 100, as specified by IPUMS
data$PERWT <- data$PERWT/100
data$HHWT <- data$HHWT/100

#Recording Wages from Income (INCWAGE) and Household income (HHINCOME)
data$INCWAGE[data$INCWAGE == 999999] <- NA
data$INCWAGE[data$INCWAGE == 999998] <- NA
data$HHINCOME[data$HHINCOME == 999999] <- NA
data$INCWAGE %<>% as.integer()
data$POVERTY %<>% as.integer()


##Subsetting City by the CITY and COUNTYFIP Code to the west coast 7. It is important to note that the cities we are looking at are dispersed in two differen tcolumns. Adidtionally, some cities have a larger observation/sample size compared ot others. 
allCity <- data %>% subset(CITY == "6430" | 
                              CITY == "6290" | 
                              CITY == "5530" | 
                              COUNTYFIP == "73" | 
                              COUNTYFIP == "67" | 
                              COUNTYFIP == "19")

#Removing the initial "data" 
rm(data)
#Removing all cities that are not the ones we are interested in. 
allCity$CITY <- replace(allCity$CITY, allCity$CITY != "6430" & 
                     allCity$CITY != "6290" & 
                     allCity$CITY != "5530", NA)

allCity$COUNTYFIP <- replace(allCity$COUNTYFIP,
                               allCity$COUNTYFIP != "73" & 
                               allCity$COUNTYFIP != "67" & 
                               allCity$COUNTYFIP != "19", NA)

allCity$COUNTYFIP %<>% as.integer()
allCity$CITY %<>% as.integer()
allCity %<>% mutate(City = coalesce(COUNTYFIP,CITY))

#Recoding cities to their names. 
allCity$City[allCity$City == "6430"] <- "Seattle"
allCity$City[allCity$City == "6290"] <- "San Francisco"
allCity$City[allCity$City == "5530"] <- "Portland"
allCity$City[allCity$City == "73"] <- "San Diego"
allCity$City[allCity$City == "67"] <- "Sacramento"
allCity$City[allCity$City == "19"] <- "Fresno"

# Temporary code just to see what each city's dataframe looks like.
# seattle <- data %>% subset(CITY == "6430")
# sanFrancisco <- data %>% subset(CITY == "6290")
# portland <- data %>% subset(CITY == "5530")
# sanDiego <- data %>% subset(COUNTYFIP == "73")
# sacramento <- data %>% subset(COUNTYFIP == "67")
# fresno <- data %>% subset(COUNTYFIP == "19")

##creating backup of AllCity, as allcity is a 5 gig dataset
allCitybackup <- allCity 


#Just making sure household income is coded properly.
allCity$HHINCOME %<>% as.integer()
allCity$HHINCOME[allCity$HHINCOME == 9999999] <- NA

#Test to see if income is being properly reported. 
allCity %>% group_by(City, YEAR) %>% 
  summarize(m = mean(INCWAGE,na.rm = TRUE))

##Migration Code###

#Recoding 0 to NA, as specified in IPUMS codebook
allCity$MIGRATE1 <- replace(allCity$MIGRATE1, allCity$MIGRATE1 == 0, NA)
allCity$MIGRATE1 %<>% as.factor()
allCity$YEAR %<>% as.integer()

Migration <- allCity %>%
  group_by(City, MIGRATE1, YEAR) %>%
  summarise(Percentage = n()) %>%
  group_by(City, YEAR) %>%
  mutate(Percentage=Percentage/sum(Percentage)) %>%
  as.data.frame()

Migration1 <- Migration

#Renaming the MIGRATE variable into its proper factors. 
Migration$MIGRATE1 %<>% as.integer()
Migration$MIGRATE1[Migration$MIGRATE1 == 1] <- "Same House"
Migration$MIGRATE1[Migration$MIGRATE1 == 2] <- "Moved Within State"
Migration$MIGRATE1[Migration$MIGRATE1 == 3] <- "Moved Between States"
Migration$MIGRATE1[Migration$MIGRATE1 == 4] <- "Abroad One Year Ago"

#Migration plot for all cities
ggplot(Migration, aes(x = YEAR, y= Percentage, group = MIGRATE1, fill = MIGRATE1)) +
         geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
         scale_x_continuous(breaks =seq(2008,2018, by=1)) +
        theme(axis.text.x = element_text(angle = 75, hjust= 1)) +
         facet_grid(. ~ City) +
    scale_y_continuous(labels = scales::percent) +
  xlab("Year") +
  ylab("Percentage (%)") +
  guides(fill=guide_legend(title="Migration Status")) +
  theme(legend.position = "bottom")

migratePercent <- Migration1 %>%
  filter(YEAR %in% c(min(YEAR), max(YEAR))) %>%
  group_by(City, MIGRATE1, YEAR) %>%
mutate(migrateStatus = case_when(MIGRATE1 == 1 | MIGRATE1 == 2 ~ 0, MIGRATE1 == 3 | MIGRATE1 ==4 ~ 1)) %>%
  filter(migrateStatus == 1) %>%
  spread(key = YEAR, value = Percentage) %>%
  ungroup() %>%
  select(-MIGRATE1) %>%
  group_by(City) %>%
  summarise_each(funs(sum))

#attempt at dynamically plotting based on column name. 
# ggplot(migratePercent, aes_string(x = colnames(migratePercent)[1], y = colnames(migratePercent)[4], fill = "City")) +
#   geom_bar(stat = "identity")

#Migration plot for all cities, year 2018. 
ggplot(migratePercent, aes(City, `2018`, fill = City)) +
         geom_bar(stat = "identity", width = .5) +
  xlab("City") +
  ylab("Proportion of non-residents (%)") +
  scale_y_continuous(labels = scales::percent, breaks = pretty_breaks()) +
  theme(legend.position = "none")
  
  # %>%mutate("Total Migrants" =Reduce("+",.[3:4]))



###Income from Wages###

allCity$AGE %<>% as.integer()

#CAGR Function. 
CAGR <- function(FV, PV, yrs = 8) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}

#Filtered out individuals younger than 20 and older than 65. 
Income <- allCity %>% 
  filter(AGE>=20 & AGE<=65, INCWAGE != 0) %>%
  group_by(City, YEAR) %>%
  summarise(medIncome = weightedMedian(INCWAGE, PERWT, na.rm = TRUE),
      meanIncome = weighted.mean(INCWAGE, PERWT, na.rm = TRUE))

IncomeCagr <- Income %>%
  select(-meanIncome) %>%
  group_by(City, YEAR) %>%
  spread(key = YEAR, value = medIncome) %>%
  mutate(CAGR = CAGR(`2018`,`2008`,10))


incomeDensity <- allCity %>% 
  filter(AGE>=20 & AGE<=65, INCWAGE != 0, City == "Seattle") %>%
  group_by(City, YEAR) %>%
  select(YEAR, INCWAGE, City)

incomedensitySeattle <- allCity %>% 
  filter(AGE>=20 & AGE<=65, INCWAGE != 0) %>%
  group_by(City, YEAR) %>%
  select(YEAR, INCWAGE, City)

incomeDensity$YEAR %<>% as.factor  

##Attempt at income density plot. 
# ggplot(data=subset(incomeDensity, YEAR == 2008 | YEAR == 2018), aes(x =
# INCWAGE, fill = YEAR)) + geom_density(alpha = .25)+ # ylab("Density") +
# xlab("Years") + theme(axis.text.y = element_blank()) + ylab("") +
# theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
# scale_x_log10(labels = dollar, breaks = trans_breaks("log10", function(x)
# 10^x)) # scale_y_continuous(breaks = seq(0,2, by = .1)) +


#CAGR Plot for income from wages
ggplot(IncomeCagr, aes(x = `2018`, y = CAGR, color = City)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = pretty_breaks(), labels = dollar) +
  ylab("CAGR, 2008-2018 (%)") +
  xlab("2018 Median Salary ($)") +
  theme(legend.position = "bottom")

ggplot(data=subset(incomeDensity, YEAR == 2008 | YEAR == 2018), aes(x = INCWAGE, fill = YEAR))+
  geom_density(alpha = .25) +
  # ylab("Density") +
  xlab("Dollars") +
  theme(axis.text.y = element_blank()) +
  ylab("") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(labels = dollar) +
  facet_grid(.~ City) +
  guides(fill=guide_legend(title="Year"))
  # scale_y_continuous(breaks = seq(0,2, by = .1)) +

#Median Income##
ggplot(Income, aes(x = YEAR, y = medIncome)) +
  geom_area(stat = "identity", fill = "#003ba0") +
  facet_grid(. ~ City) +
  ylab("Dollar") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = pretty_breaks())

##Mean Income###
ggplot(Income, aes(x = YEAR, y = meanIncome)) +
  geom_area(stat = "identity", fill = "#003ba0") +
  facet_grid(. ~ City) +
  ylab("Dollar") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = seq(10000,100000,15000))


##Seattle Income, 2008 to 2018##
seattleIncome <- Income %>%
  ungroup() %>%
  filter(YEAR %in% c(min(YEAR), max(YEAR))) %>%
    filter(City == "Seattle")

seattleIncome$YEAR %<>% as.factor()
  
ggplot(seattleIncome, aes(YEAR, medIncome)) +
  geom_bar(stat = "identity", fill = "#F564E3", width = .5) +
  scale_y_continuous(breaks = pretty_breaks(), labels = dollar) +
  xlab("Year") +
  ylab("Median annual income ($)")

ggplot(seattleIncome, aes(YEAR, meanIncome)) +
  geom_bar(stat = "identity", fill = "#F564E3", width = .5) +
  scale_y_continuous(breaks = pretty_breaks(), labels = dollar) +
  xlab("Year") +
  ylab("Mean annual income ($)")


###Household Income###
hhIncome <- allCity %>% 
  group_by(City, YEAR) %>%
  summarise(medIncome = weightedMedian(HHINCOME, HHWT, na.rm = TRUE),
            meanIncome = weighted.mean(HHINCOME, HHWT, na.rm = TRUE))

ggplot(hhIncome, aes(x = YEAR, y = medIncome)) +
  geom_area(stat = "identity")+
  facet_grid(. ~ City) +
  ylab("Dollars") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = pretty_breaks())


ggplot(hhIncome, aes(x = YEAR, y = meanIncome)) +
  geom_area(stat = "identity")+
  facet_grid(. ~ City) +
  ylab("Salary per year") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = seq(0,500000, by = 25000))



###Population Pyramid###
allCity$SEX[allCity$SEX == 1] <- "Male"
allCity$SEX[allCity$SEX == 2] <- "Female"
allCity$SEX %<>% as.factor()

age <- allCity %>%
  group_by(City, SEX, YEAR) %>%
  summarise(medAge = weightedMedian(AGE, PERWT, na.rm = TRUE),
            meanAge = weighted.mean(AGE, PERWT, na.rm = TRUE))

ggplot(data=subset(allCity, City == "Seattle"),aes(x = AGE,fill=SEX)) + 
  geom_bar(data=subset(allCity, SEX=="Female"), width = .5) + 
  geom_bar(data=subset(allCity,SEX=="Male"),aes(y=..count..*(-1)), width = .5) + 
  scale_y_continuous(breaks = seq(-10000,10000,5000), labels=abs(seq(-10000,10000,5000))) +
  scale_x_continuous(breaks = seq(0,100, 10)) +
  coord_flip()

ggplot(data=subset(allCity, YEAR == "2018"),aes(x = SEX, y = AGE, fill = City)) + 
  geom_violin() +
  scale_y_continuous(breaks = seq(0,100, by = 15)) + 
  geom_boxplot(aes(x = SEX, y = AGE), width = .25) +
  facet_grid(. ~ City) +
  xlab("Gender") +
  ylab("Age") +
  theme(legend.position = "bottom")


###Proportion of Renters####

allCity$OWNERSHP[allCity$OWNERSHP == 0] <- NA

rent <- allCity %>%
  group_by(City, YEAR, OWNERSHP) %>%
  drop_na(OWNERSHP) %>%
  summarise(count = n()) %>%
  mutate( prop = count / sum(count) )

rent$OWNERSHP %<>% as.factor()


rent2018 <- rent %>%
  filter(YEAR %in% c(min(YEAR), max(YEAR)))
  
rent2018$OWNERSHP %<>% as.numeric()


ggplot(rent, aes(x = YEAR, y= prop, group = OWNERSHP, fill = OWNERSHP)) +
  geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  theme(axis.text.x = element_text(angle = 75, hjust= 1)) +
  facet_grid(. ~ City) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Ownership Status",
                     labels = c("Owned", "Rented"), values = c("darksalmon", "blue")) +
  xlab("Year") +
  ylab("Proportion")

ggplot(subset(rent2018, YEAR %in% max(YEAR)), aes(x = YEAR, y= prop, fill = City, alpha = OWNERSHP)) +
  geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  facet_grid(. ~ City) +
  scale_y_continuous(labels = scales::percent) +
  scale_alpha(range=c(0,1), limits=c(0,2)) +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 3) +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Proportion")

rent2018$OWNERSHP %<>% as.factor()

ggplot(subset(rent2018, YEAR %in% max(YEAR)), aes(x = YEAR, y= prop, fill = OWNERSHP)) +
  geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  facet_grid(. ~ City) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 3) +
  xlab("Year") +
  ylab("Proportion") + 
  scale_fill_manual(name = "Ownership Status",
                    labels = c("Owned", "Rented"), values = c("darksalmon", "blue"))
    

###Race Proportions###
race <- allCity %>%
  group_by(City, YEAR, RACE) %>%
  drop_na(RACE) %>%
  summarise(count = n()) %>%
  mutate( prop = count / sum(count) )

race$RACE %<>% as.integer()

race$RACE[race$RACE == 1] <- "White"
race$RACE[race$RACE == 2] <- "Black/African American"
race$RACE[race$RACE == 3] <- "American Indian/Alaska Native"
race$RACE[race$RACE == 4] <- "Chinese"
race$RACE[race$RACE == 5] <- "Japanese"
race$RACE[race$RACE == 6] <- "Other Asian/Pacific Islander"
race$RACE[race$RACE == 7] <- "Other race"
race$RACE[race$RACE == 8] <- "Two major races"
race$RACE[race$RACE == 9] <- "Three or more major races"

ggplot(subset(race, YEAR %in% c(2008,2018)), aes(YEAR, prop, fill = RACE, group = RACE)) +
  geom_bar(stat = "identity") +
  facet_grid(.~City) +
  scale_x_continuous(breaks =c(seq(2008,2018,by = 10))) +
  scale_fill_brewer(palette="Paired", name = "Race") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Year") +
  ylab("Proportion (%)") +
  theme(legend.position  = "bottom") +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))


seattleRace <- race %>%
filter(City == "Seattle")

ggplot(subset(seattleRace, YEAR %in% c(2008,2018)), aes(YEAR, prop, fill = RACE, group = RACE)) +
  geom_bar(stat = "identity") +
  facet_grid(.~City) +
  scale_x_continuous(breaks =c(seq(2008,2018,by = 10))) +
  scale_fill_brewer(palette="Paired", name = "Race") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  xlab("Year") +
  ylab ("Percent") +
  theme(legend.position = "bottom")
  

allCity$RACE %<>% as.integer()

Diversity <- allCity %>%
  drop_na(RACE) %>%
  filter(YEAR %in% c(min(YEAR), max(YEAR))) %>%
  mutate(diversity = case_when(RACE >1 ~ "POC", RACE == 1 ~ "White")) %>%
  group_by(City, YEAR, diversity) %>%
  summarise(count = n()) %>%
  mutate( prop = count / sum(count) ) %>%
  filter(diversity == "POC") %>%
  spread(key = YEAR, value = prop) %>%
  select(-c(count, diversity)) %>%
  group_by(City) %>%
  summarise_each(funs(na.omit)) %>%
  mutate(diversechange = (`2018` - `2008`))
  

ggplot(Diversity, aes(`2018`, diversechange, color = City)) +
  geom_point() +
  xlab("% People of Color") +
  ylab("% Change in diversity from 2008-2018") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")



###Average Rent Cost####
allCity$RENT %<>% as.integer()
allCity$RENT[allCity$RENT==0] <-NA
rentPayment <- allCity %>% 
  group_by(City, YEAR) %>%
  summarise(medRent = weightedMedian(RENT, HHWT, na.rm = TRUE),
            meanRent = weighted.mean(RENT, HHWT, na.rm = TRUE))

rentPaymentCAGR <- rentPayment %>%
select(-meanRent) %>%
  spread(key = YEAR, value = medRent) %>%
  mutate(CAGR = CAGR(`2018`,`2008`,10))


  

ggplot(rentPayment,
              aes(x = YEAR, y = medRent)) +
  geom_bar(stat = "identity")+
  facet_grid(. ~ City) +
  ylab("Dollars/Month") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = pretty_breaks()) +
  ggtitle("Median Monthly Rent By Year and City")

ggplot(rentPayment, aes(x = YEAR, y = meanRent)) +
  geom_bar(stat = "identity", fill = "#003ba0")+
  facet_grid(. ~ City) +
  ylab("Dollars") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = pretty_breaks())


ggplot(rentPaymentCAGR, aes(x = `2018`, y = CAGR, color = City)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = pretty_breaks(), labels = dollar) +
  ylab("CAGR, 2008-2018 (%)") +
  theme(legend.position = "bottom") +
  xlab("Median Rent Payment") 


#####RENT BURDEN####
allCity$RENT %<>% as.integer()
allCity$RENT[allCity$RENT==0] <-NA
rentBurden <- allCity %>% 
  filter(AGE>=20 & AGE<=65, INCWAGE != 0) %>%
  group_by(City, YEAR) %>%
  summarise(medRent = weightedMedian(RENT, HHWT, na.rm = TRUE),
            meanRent = weighted.mean(RENT, HHWT, na.rm = TRUE),
            medIncome = weightedMedian(INCWAGE, PERWT, na.rm = TRUE),
            meanIncome = weighted.mean(INCWAGE, PERWT, na.rm = TRUE)) %>%
  mutate(prop = medRent/(medIncome/12))

rentBurden$YEAR %<>% as.factor

ggplot(data = subset(rentBurden,YEAR %in% c(2008,2018)), aes(YEAR, prop, fill = YEAR)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ City) +
  xlab("Year") +
  ylab("Percent of Income towards Rent") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Year",
                     values = c("darksalmon", "blue")) +
  xlab("year") +
  ylab("Percent of Income towards Rent")

####RENT BURDEN BY INCOME DECILE####
allCity$HHINCOME %<>% as.integer()
allCity$HHINCOME[allCity$HHINCOME == 9999999] <- NA
allCity$HHINCOME[allCity$HHINCOME < 0] <- NA
allCity$RENT %<>% as.integer()
allCity$RENT[allCity$RENT==0] <-NA
rentBurdenDecile <- allCity %>% 
  filter(AGE>=20 & AGE<=65, INCWAGE != 0) %>%
  mutate(decile = ntile(HHINCOME, 10), rentBurden = RENT/(HHINCOME/12)) %>%
  group_by(City,YEAR, decile) %>%
  summarise(mean = weighted.mean(rentBurden, HHWT, na.rm = TRUE),
            median = weightedMedian(rentBurden, HHWT, na.rm = TRUE))

rentBurdenDecile$YEAR %<>% as.integer()
rentBurdenDecile$decile %<>% as.factor()

rentQuantile <- setDT(as.data.frame(wtd.quantile(allCity$HHINCOME, seq(0,1,.1),na.rm = TRUE, weight = allCity$HHWT)), keep.rownames = TRUE, check.names = TRUE)

colnames(rentQuantile) <- c("Percent","Income")

  
ggplot(data = subset(rentBurdenDecile,YEAR %in% c(2008,2018)), aes(YEAR, median, fill = decile)) +
  geom_bar(stat = "identity") +
  facet_grid(.~City) +
  scale_x_continuous(breaks =c(seq(2008,2018,by = 10))) +
  geom_text(aes(label = paste(round(median*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 2.5) +
  scale_color_brewer(palette="Paired") +
  xlab("year") +
  ylab("Percent of Income towards Rent") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="Income Decile"))
  
  

ggplot(rentPayment,
       aes(x = YEAR, y = medRent)) +
  geom_bar(stat = "identity")+
  facet_grid(. ~ City) +
  ylab("Dollars/Month") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous(labels = dollar, breaks = pretty_breaks()) +
  ggtitle("Median Monthly Rent By Year and City")

incomeDensity <- allCity %>% 
  filter(AGE>=20 & AGE<=65, INCWAGE != 0, City == "Seattle") %>%
  group_by(City, YEAR) %>%
  select(YEAR, INCWAGE, City)

######PART TWO######

#total Population

population <- read.csv("Age_Data_Edited.csv", check.names = FALSE)

populationCAGR <- 
  population %>%
  mutate(CAGR = CAGR(`2018`,`2010`,8))

population <- gather(population, key = "Year", value = "Population",2:10)

ggplot(population, aes(x = Year, y =Population)) +
  geom_bar(stat = "identity",fill = "#003ba0") +
  facet_grid(. ~ City) +
  ylab("Population") +
  scale_y_continuous(labels = comma, breaks = seq(0,1500000,by = 200000)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  theme(legend.position = "bottom")





ggplot(populationCAGR, aes(x = `2018`, y = CAGR, color = City)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = pretty_breaks(), labels = comma) +
  ylab("CAGR, 2008-2018 (%)") +
  theme(legend.position = "bottom")
  

#Household Size

hhSize <- allCity %>% 
  group_by(SERIAL) %>%
  group_by(City, YEAR) %>%
  summarise(meanHHsize = weighted.mean(FAMSIZE, HHWT, na.rm = TRUE))

ggplot(data=subset(hhSize), aes(x = YEAR, y = meanHHsize)) +
  geom_bar(stat = "identity", fill = "#003ba0")+
  facet_grid(. ~ City) +
  ylab("Size of Household") +
  xlab("Years") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous( breaks = seq(0,3, by = .2))

hhsize2018 <- hhSize %>%
  filter(YEAR == "2018")

ggplot(hhsize2018, aes(City, meanHHsize, fill = City)) +
  geom_bar(stat = "identity", width = .5) +
  xlab("City") +
  ylab("Household Size") +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = pretty_breaks())



#commute time
allCity$TRANTIME[allCity$TRANTIME == 0] <- NA
                     
transit <- allCity %>% 
  group_by(City, YEAR) %>%
  summarise(Time = weighted.mean(TRANTIME, PERWT, na.rm = TRUE)) 


ggplot(data=subset(transit), aes(x = YEAR, y = Time)) +
  geom_area(stat = "identity", fill = "#003ba0")+
  facet_grid(. ~ City) +
  ylab("Transit Time (Minutes)") +
  xlab("Year") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_continuous(breaks =seq(2008,2018, by=1)) +
  scale_y_continuous( breaks = seq(0,40, by = 5))

##Commute Time Distribution###

commuteDensity <- allCity %>% 
  filter(City == "Seattle") %>%
  group_by(YEAR)

commuteDensity$YEAR %<>% as.factor()

mu <- plyr::ddply(commuteDensity, "YEAR", summarise, meanCommute  = weighted.mean(TRANTIME, PERWT, na.rm = TRUE))


ggplot(data=subset(commuteDensity, YEAR == 2008 | YEAR == 2018), aes(x = TRANTIME, color = YEAR))+
  geom_density(alpha = .25) +
  # ylab("Density") +
  xlab("Commute Time") +
  theme(axis.text.y = element_blank()) +
  ylab("") +
  scale_x_continuous(breaks = seq(0,200, by = 10)) +
 facet_grid(.~ City) +
   geom_vline(data=subset(mu, YEAR == 2008 | YEAR == 2018), aes(xintercept=meanCommute, color = YEAR)) +
  geom_text(data=subset(mu, YEAR == 2008 | YEAR == 2018), mapping = aes(x = meanCommute, label = meanCommute, y = 0), vjust = -1, hjust = .1, angle = 60)





##Languages Spoken###

allCity <- allCitybackup

allCity$LANGUAGE[allCity$LANGUAGE == 0] <- NA
allCity <- allCity[!is.na(allCity$LANGUAGE), ]

languageEnglish <- allCity %>%
  mutate_at(
    vars("LANGUAGE"),
    funs(as_factor(.))) %>%
  group_by(City, YEAR, LANGUAGE) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  mutate(LANGUAGE = replace(LANGUAGE, prop <=.015, "Other or not reported")) %>%
  ungroup() %>%
  group_by(City, YEAR, LANGUAGE) %>%
  summarise_each(funs(sum))

language <- allCity %>%
  mutate_at(
    vars("LANGUAGE"),
    funs(as_factor(.))) %>%
  group_by(City, YEAR, LANGUAGE) %>%
  filter(LANGUAGE != "English") %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  #"Other or not reported" Is the exact same as the ACS factor. This is necessary - if this changes.
  mutate(LANGUAGE = replace(LANGUAGE, prop <=.015, "Other or not reported")) %>%
  ungroup() %>%
  group_by(City, YEAR, LANGUAGE) %>%
    summarise_each(funs(sum))



###Seattle non-major  Languages##
languageSeattle <- language %>%
  filter(City == "Seattle")

languageSeattle$YEAR %<>% as.factor

mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(18)

ggplot(subset(languageSeattle, YEAR == "2018"), aes(x = YEAR, y = prop, fill = LANGUAGE)) +
  geom_bar(stat = "identity", width =.3) +
  scale_fill_manual(values = mycolors,name = "Language") +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(.~City)+ 
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 2) +
  xlab('Year') +
  ylab("Percentage")

###English Comparison###
languageEnglishOther <- allCity %>%
  mutate_at(
    vars("LANGUAGE"),
    funs(as_factor(.))) %>%
  group_by(City, YEAR, LANGUAGE) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  filter(LANGUAGE == "English") %>%
  ungroup() %>%
  select(-LANGUAGE, -count)%>%
  filter(YEAR %in% max(YEAR))
  

ggplot(languageEnglishOther, aes(City, prop, fill = City)) +
  geom_bar(stat = "identity", width = .5) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 2.5) +
  xlab("City") +
  ylab("Proportion of English Spoken(%)") +
  theme(legend.position = "none")
  
ggplot(subset(languageEnglish, YEAR %in% c(2008,2018)), aes(x = YEAR, y = prop, fill = LANGUAGE, group = LANGUAGE)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks =c(seq(2008,2018,by = 10))) +
  scale_fill_brewer(palette="Paired") +
  facet_grid(. ~ City) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep= "")), position = position_stack(vjust = 0.5), size = 2) +
  xlab('Year') +
  ylab("Language Percentage") +
  theme(legend.position = "bottom")


mycolors1 <- colorRampPalette(brewer.pal(12, "Paired"))(24)
  

ggplot(subset(language, YEAR %in% c(2008,2018)), aes(x = YEAR, y = prop, fill = LANGUAGE, group = LANGUAGE)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks =c(seq(2008,2018,by = 10))) +
  scale_fill_manual(values = mycolors1,name = "Language") +
  facet_grid(. ~ City) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep= "")), position = position_stack(vjust = 0.5), size = 2) +
  xlab('Year') +
  ylab("Language Percentage") +
  theme(legend.position = "bottom")


###Proportion of Households With Children###
allCity$NCHILD %<>% as.integer()

hhChildren <- allCity %>%
  mutate(childProp = case_when(NCHILD >=1 ~ 1, NCHILD == 0 ~ 0)) %>%
  group_by(City, YEAR, childProp) %>%
summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  filter(YEAR == 2018)


ggplot(hhChildren, aes(City, prop, fill = City, alpha = prop)) +
  geom_bar(stat = "identity", position = "stack", width = .5) +
  theme(legend.position = "none") +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  xlab("City") +
  ylab("Proportion of Houesholds without Children (%)") +
  scale_alpha(range=c(0,1), limits=c(0,1), na.value = 0)

  

###EDUCATIOn


# Education <- allCity %>% 
#   filter(AGE>=20 & AGE<=65, INCWAGE != 0) %>%
#   mutate(decile = ntile(INCWAGE, 10), rentBurden = RENT/(INCWAGE/12)) %>%
#   group_by(City, YEAR) %>%
#   select(YEAR, City, INCWAGE, EDUC, decile, PERWT) %>%
#   summarise(medEduc = weightedMedian(EDUC, PERWT, na.rm = TRUE),
#             meanEduc = weighted.mean(EDUC, PERWT, na.rm = TRUE)) 
#   filter(City == "Seattle")

  
  
Education <- allCity %>% 
  mutate_at(
    vars("EDUC"),
    funs(as_factor(.))) %>%
  group_by(City, YEAR, EDUC) %>%
  summarise(count = n()) %>%
    mutate( prop = count / sum(count) ) %>%
  ungroup() %>%
  filter(YEAR %in% c(min(YEAR), max(YEAR))) %>%
  mutate_at(vars("YEAR"),
             funs(factor))


#Education for all Cities  
ggplot(Education, aes(YEAR, prop, fill = EDUC)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 2) +
  facet_grid(.~City) +
  xlab("Year") +
  ylab("Education Distribution (%(") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="Educational Attainment"))

#Seattle Education
  
ggplot(subset(Education, City == "Seattle"), aes(YEAR, prop, fill = EDUC)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label = paste(round(prop*100, digits =2), "%", sep="")), position = position_stack(vjust = 0.5), size = 2) +
  facet_grid(.~City) +
  xlab("Year") +
  ylab("Education Distribution (%(") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="Educational Attainment"))
            
