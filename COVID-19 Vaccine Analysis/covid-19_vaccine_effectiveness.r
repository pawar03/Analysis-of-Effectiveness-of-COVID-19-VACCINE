#Required packages
library(tidyverse)
library(ggplot2)

covid_data <- read.csv("owid-covid-data.csv")
nz <- covid_data %>% 
  filter(iso_code == "NZL")
head(nz)
str(nz)
nz_filter <- nz %>% select(iso_code, date, total_cases, new_cases, total_deaths, new_deaths, total_vaccinations, people_vaccinated, people_fully_vaccinated, new_vaccinations, population)


#Local records only after vaccination has started
nz_vac_filter <- nz_filter[!is.na(nz_filter$total_vaccinations),]
#Renumber the row
rownames(nz_vac_filter) <- NULL
#Local records that have incompleted fields
nz_vac_filter[!complete.cases(nz_vac_filter),]
#Replace incompleted fields with 0
nz_vac_filter[is.na(nz_vac_filter$people_fully_vaccinated),"people_fully_vaccinated"] <- 0
nz_vac_filter[is.na(nz_vac_filter$new_vaccinations),"new_vaccinations"] <- 0
head(nz_vac_filter)


#Format date in the data
nz_vac_filter$Posixdate <- as.POSIXct(nz_vac_filter$date, format="%Y-%m-%d")
#Calculate percentage of vaccinated people
nz_vac_filter$PercentVaccine <- nz_vac_filter$people_vaccinated / nz_vac_filter$population *100
str(nz_vac_filter)

p1 <- ggplot(data = nz_vac_filter, aes(x=Posixdate,y=PercentVaccine, colour=iso_code))
p1 + geom_line(size=1.2) + 
  scale_y_continuous(breaks=c(0,20,40,60),labels = scales::comma) +
  labs(title = "% of vaccinated people over time in New Zealand", 
       x='Date', 
       y='% vaccinated people')

p2 <- ggplot(data = nz_vac_filter, aes(x=Posixdate,y=total_cases, colour=iso_code))
p2 + geom_line(size=1.2) + 
  labs(title = "Total number of cases over time in New Zealand", 
       x='Date', 
       y='Total number of cases')


#Repeat process with added countries
comb <- covid_data %>% 
  filter(iso_code %in% c("NZL","GBR","USA","ISR","BRA","IND"))
comb_filter <- comb %>% select(iso_code, date, total_cases, new_cases, total_deaths, new_deaths, total_vaccinations, people_vaccinated, people_fully_vaccinated, new_vaccinations, population)

#Local records only after vaccination has started
comb_vac_filter <- comb_filter[!is.na(comb_filter$total_vaccinations),]
rownames(comb_vac_filter) <- NULL
comb_vac_filter[!complete.cases(comb_vac_filter),]
comb_vac_filter[is.na(comb_vac_filter$people_fully_vaccinated),"people_fully_vaccinated"] <- 0
comb_vac_filter[is.na(comb_vac_filter$new_vaccinations),"new_vaccinations"] <- 0


#USA has a missing data on 13JAN2021, BRA has missing data on 23JUN2021
comb_vac_filter[!complete.cases(comb_vac_filter),]

#Replace with average of +1/-1 date
comb_vac_filter[comb_vac_filter$iso_code=="USA" & comb_vac_filter$date=="2021-01-13", "people_vaccinated"] <- (comb_vac_filter[comb_vac_filter$iso_code=="USA" & comb_vac_filter$date=="2021-01-12", "people_vaccinated"]+comb_vac_filter[comb_vac_filter$iso_code=="USA" & comb_vac_filter$date=="2021-01-14", "people_vaccinated"])/2
comb_vac_filter[comb_vac_filter$iso_code=="BRA" & comb_vac_filter$date=="2021-06-22", "people_vaccinated"] <- (comb_vac_filter[comb_vac_filter$iso_code=="BRA" & comb_vac_filter$date=="2021-06-21", "people_vaccinated"]+comb_vac_filter[comb_vac_filter$iso_code=="BRA" & comb_vac_filter$date=="2021-06-23", "people_vaccinated"])/2
#check:
comb_vac_filter[765,]
comb_vac_filter[140,]

#Format date in the data
comb_vac_filter$Posixdate <- as.POSIXct(comb_vac_filter$date, format="%Y-%m-%d")

#Calculate percentage of vaccinated people
comb_vac_filter$PercentVaccine <- comb_vac_filter$people_vaccinated / comb_vac_filter$population *100

p3 <- ggplot(data = comb_vac_filter, aes(x=Posixdate,y=PercentVaccine, colour=iso_code))
p3 + geom_line(size=1.1)+ 
  scale_y_continuous(breaks=c(0,20,40,60,80,100),labels = scales::comma) + 
  labs(title="% vaccinated people over time", x="Date", y="% vaccinated people", colour = "Location")

p4 <- ggplot(data = comb_vac_filter, aes(x=Posixdate,y=total_cases, colour=iso_code))

p4 + geom_line(size=1.1) +
  facet_grid(iso_code~., scales = "free") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Total number of cases over time", 
       x='Date', 
       y='Total number of cases',
       colour = "Location")

