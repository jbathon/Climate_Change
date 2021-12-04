rm(list=ls())
library(tidyverse)
library(plotly)

library(gganimate)
library(gifski)

options(scipen=999)

# Task 1

# Plot 1

globalTemps <-  read.csv("GlobalLandTemperaturesByCity.csv")

globalTemps2 <- globalTemps %>% 
  separate(dt, into=c("year","month","day"), sep="-") %>% 
  mutate(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day)) %>% 
  filter(year >= 1875 & !is.na(AverageTemperature))

globalTemps2 <- read.csv("GlobalLandTemperaturesByCity1875.csv")

write.csv(globalTemps2, file="GlobalLandTemperaturesByCity1875.csv")

globalTemps3 <- globalTemps2 %>%
  group_by(year) %>% 
  summarise(
    meanTemp = mean(AverageTemperature), 
    sdTemp = sd(AverageTemperature),
    seTemp = sd(AverageTemperature)/n(),
    moeTemp = qnorm(.975)*sd(AverageTemperature)/sqrt(n()),
    lower = mean(AverageTemperature)-(qnorm(.975)*sd(AverageTemperature)/sqrt(n())),
    upper = mean(AverageTemperature)+(qnorm(.975)*sd(AverageTemperature)/sqrt(n()))
  )


meanTemp <-  
  ggplot(globalTemps3, aes(x=year, y=meanTemp)) +
    geom_point(alpha=.75) +
    geom_smooth(method = lm, color="red") +
    geom_linerange(aes(ymin = lower, ymax = upper), alpha=.75) +
    labs(title = "Global Mean Temperature from 1875", x = "Year", y = "Mean Temperature (?C)")

meanTemp

ggplotly(meanTemp)

lm(meanTemp ~ year, globalTemps3)

0.009894 * 2050 - 1.519692 

# Plot 2

# Cities: Sydney, Australia; Tokyo, Japan; Paris, France; Toronto, Canada

cityTemps <- globalTemps2 %>% 
  filter(
    (City == "Sydney" & Country == "Australia") |
    (City == "Tokyo" & Country == "Japan") |
    (City == "Paris" & Country == "France") |
    (City == "Toronto" & Country == "Canada")
  ) %>% 
  mutate(City = factor(City), Country = factor(Country))

cityTemps2 <-  cityTemps %>%  
  group_by(year, City, Country) %>% 
  summarise(
    meanTemp = mean(AverageTemperature), 
    sdTemp = sd(AverageTemperature),
    seTemp = sd(AverageTemperature)/n(),
    moeTemp = qnorm(.975)*sd(AverageTemperature)/sqrt(n()),
    lower = mean(AverageTemperature)-(qnorm(.975)*sd(AverageTemperature)/sqrt(n())),
    upper = mean(AverageTemperature)+(qnorm(.975)*sd(AverageTemperature)/sqrt(n()))
  ) %>% 
  ungroup() %>% 
  rename(city = City, country = Country) %>% 
  arrange(city,year)

# Paris
paris <-  cityTemps2 %>% 
  filter(city == "Paris")

parisMean <- 
  ggplot(paris ,aes(x=year, y=meanTemp)) +
  geom_point(alpha=.5) +
  geom_smooth(method = lm) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  labs(title = "Paris Mean Temperatures from 1875", x = "Year", y = "Mean Temperature (°C)")
  
  ggplotly(parisMean)

  lm(meanTemp ~ year, paris)
# Sydney
sydney <-  cityTemps2 %>% 
  filter(city == "Sydney")


sydneyMean <- 
  ggplot(sydney ,aes(x=year, y=meanTemp)) +
  geom_point(alpha=.5) +
  geom_smooth(method = lm) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  labs(title = "Sydney Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
  
  ggplotly(sydneyMean)
  
  lm(meanTemp ~ year, sydney)

# Toyko


  tokyo <-  cityTemps2 %>% 
  filter(city == "Tokyo")

tokyoMean <- 
  ggplot(tokyo ,aes(x=year, y=meanTemp)) +
  geom_point(alpha=.5) +
  geom_smooth(method = lm) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  labs(title = "Tokyo Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
  
  ggplotly(tokyoMean)
  
  lm(meanTemp ~ year, tokyo)

# Toronto

toronto <-  cityTemps2 %>% 
  filter(city == "Toronto")

torontoMean <- 
  ggplot(toronto, aes(x=year, y=meanTemp)) +
  geom_point(alpha=.5) +
  geom_smooth(method = lm) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  labs(title = "Toronto Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
  
  ggplotly(torontoMean)
  
  lm(meanTemp ~ year, toronto)
  
# analysis
  filter(paris, year == 1875)
  filter(sydney, year == 1875)
  filter(tokyo, year == 1875)
  filter(toronto, year == 1875)
  
# Plot 3
  
  countryTemps <- globalTemps2 %>% 
    filter(
      (Country == "Australia") |
        (Country == "China") |
        (Country == "Iceland") |
        (Country == "United States")
    ) %>% 
    mutate(Country = factor(Country))
  
  countryTemps2 <-  countryTemps %>%  
    group_by(year, Country) %>% 
    summarise(
      meanTemp = mean(AverageTemperature), 
      lower = mean(AverageTemperature)-(qnorm(.975)*sd(AverageTemperature)/sqrt(n())),
      upper = mean(AverageTemperature)+(qnorm(.975)*sd(AverageTemperature)/sqrt(n()))
    ) %>% 
    ungroup() %>% 
    rename(country = Country) %>% 
    arrange(country, year)
  
  
  # Australia
  
  australia <- countryTemps2 %>% 
    filter(country == "Australia")
  
    ggplot(australia, aes(x=year, y=meanTemp)) +
    geom_point(alpha=.5) +
    geom_smooth(method = lm) +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    labs(title = "Australia Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
    
    lm(meanTemp ~ year, australia)
  
  # China
  
  china <- countryTemps2 %>% 
    filter(country == "China")
  
    ggplot(china, aes(x=year, y=meanTemp)) +
    geom_point(alpha=.5) +
    geom_smooth(method = lm) +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    labs(title = "China Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
    
    lm(meanTemp ~ year, china)
    
  # Iceland
  
  iceland <- countryTemps2 %>% 
    filter(country == "Iceland")
  
  icelandMean <- 
    ggplot(china, aes(x=year, y=meanTemp)) +
    geom_point(alpha=.5) +
    geom_smooth(method = lm) +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    labs(title = "Iceland Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
  
  ggplotly(icelandMean)
  
  lm(meanTemp ~ year, iceland)
  
  # United States
  
  unitedStates <- countryTemps2 %>% 
    filter(country == "United States")
  
  unitedStatesMean <- 
    ggplot(unitedStates, aes(x=year, y=meanTemp)) +
    geom_point(alpha=.5) +
    geom_smooth(method = lm) +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    labs(title = "United States Mean Temperature from 1875", x = "Year", y = "Mean Temperature (°C)")
  
  ggplotly(unitedStatesMean)
  
  lm(meanTemp ~ year, unitedStates)
  
  # analysis
  
  filter(australia, year == 1875)
  filter(china, year == 1875)
  filter(iceland, year == 1875)
  filter(unitedStates, year == 1875)
  
  #Task 2
  

world <- map_data("world")
globalTemps5 <- globalTemps2 %>%
  rename(region = Country) %>% 
  group_by(year, region) %>% 
  summarise(
    meanTemp = mean(AverageTemperature), 
    lower = mean(AverageTemperature)-(qnorm(.975)*sd(AverageTemperature)/sqrt(n())),
    upper = mean(AverageTemperature)+(qnorm(.975)*sd(AverageTemperature)/sqrt(n()))
  ) %>% 
  mutate(
    region = replace(region, region == "United States", "USA"),
    region = replace(region, region == "United Kingdom", "UK"),
    region = replace(region, region == "Congo", "Republic of Congo"),
    region = replace(region, region == "Congo (Democratic Republic Of The)", "Democratic Republic of the Congo"),
    region = replace(region, region == "CÃ´te D'Ivoire" , "Ivory Coast")
    )

mapTemps <- world %>% 
  left_join(globalTemps5,by="region") %>% 
  filter(!is.na(year))


p <-  ggplot(mapTemps, aes(x=long, y = lat, group = group)) +
  geom_polygon(aes(fill=meanTemp), color= "black") +
  labs(title = "Mean Temperature of Countries: {current_frame} A.D.", x = "Longitude", y = "Latitude", fill = "Mean Temperature (?C)") +
  scale_fill_viridis_c(option = "turbo", limits= c(-14,32)) + 
  transition_manual(year) +
  theme_custom()

 animate(p, nframe = 138, renderer = gifski_renderer("map.gif"), height = 500, width = 800, fps=5, end_pause = 17)

# Task 3

 #https://catalog.data.gov/dataset/national-usfs-fire-occurrence-point-feature-layer-1ea1c
 
wildfire <-  read.csv("datasets/wildfires.csv")

wildfire2 <- wildfire %>% 
  filter(FIREYEAR >= 1925 & FIREYEAR <= 2013 & FIRETYPECATEGORY == "WF") %>% 
  select(
    FIREYEAR,
    SIZECLASS,
    TOTALACRES,
    LATDD83,
    LONGDD83
  ) %>% 
  rename(
    year = FIREYEAR,
    sizeClass = SIZECLASS,
    acres = TOTALACRES,
    lat =LATDD83,
    long =LONGDD83
  )

wildfire3 <- wildfire2 %>% 
  group_by(year) %>% 
  summarise(
    count = n(), 
    totalAcres = sum(acres, na.rm=T))

unitedStatesTemp <- globalTemps2 %>% 
  filter(Country == "United States") %>% 
  group_by(year) %>% 
  summarise(meanTemp = mean(AverageTemperature))

wildfire4 <- wildfire3 %>% 
  inner_join(unitedStatesTemp, by="year")

wildfire5 <- wildfire2 %>% 
  filter(acres >= 5000) %>% 
  group_by(year) %>% 
  summarise(
    count = n(), 
    totalAcres = sum(acres, na.rm=T)) %>% 
  inner_join(unitedStatesTemp, by="year")

wildfire6 <- wildfire2 %>% 
  filter(acres <= 1000) %>% 
  group_by(year) %>% 
  summarise(
    count = n(), 
    totalAcres = sum(acres, na.rm=T)) %>% 
  inner_join(unitedStatesTemp, by="year")

# Total Number of Wildfires
ggplot(wildfire4, aes(x=year,y=count,color=meanTemp)) +
  geom_point(size=2) +
  labs(title= "USA Wildfires from 1925", x="Year", y="Number of Wildfires", color="Mean Temperature (?C)") +
  geom_smooth(color ="red") +
  scale_color_viridis_c(option = "inferno") 

# Total Number of Over 5000
ggplot(wildfire5, aes(x=year,y=count,color=meanTemp)) +
  geom_point(size=2) +
  labs(title= "USA Wildfires Over 5000 acres from 1925", x="Year", y="Number of Wildfires", color="Mean Temperature (°C)") +
  geom_smooth(color ="red") +
  scale_color_viridis_c(option = "inferno")
# Total Number of Under 5000
ggplot(wildfire6, aes(x=year,y=count,color=meanTemp)) +
  geom_point(size=2) +
  labs(title= "USA Wildfires Under 1000 acres from 1925", x="Year", y="Number of Wildfires", color="Mean Temperature (°C)") +
  geom_smooth(color ="red") +
  scale_color_viridis_c(option = "inferno")


# Cumulative Acres of Wildfires per Year

ggplot(wildfire4, aes(x=year,y=totalAcres,color=meanTemp)) +
  geom_point(size=2) +
  labs(title= "Cumulative USA Wildfire Acres from 1925", x="Year", y="Cumulative Acres", color="Mean Temperature (?C)") +
  geom_smooth(color ="red") +
  scale_color_viridis_c(option = "inferno")

lm(totalAcres ~ year, filter(wildfire4, year >= 1980))

# Number of Wildfires per Mean Temp

ggplot(wildfire4, aes(x=meanTemp,y=count)) +
  geom_point(size=2) +
  labs(title= "USA Wildfire Acres versus Mean Temperture", x="Mean Temperture (?C)", y="Cumulative Acres") +
  geom_smooth(method = lm, color ="red") +
  scale_color_viridis_c(option = "inferno")

lm(count ~ meanTemp, wildfire4)

# Cumulative Acres of Wildfires per Mean Temp
ggplot(wildfire4, aes(x=meanTemp,y=totalAcres)) +
  geom_point(size=2) +
  labs(title= "Cumulative USA Wildfire Acres versus Mean Temperture", x="Mean Temperture (?C)", y="Cumulative Acres") +
  geom_smooth(method = lm, color ="red") +
  scale_color_viridis_c(option = "inferno")

lm(totalAcres ~ meanTemp, wildfire4)
