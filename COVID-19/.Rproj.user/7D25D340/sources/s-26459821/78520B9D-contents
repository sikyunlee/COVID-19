#########################################################
# COVID-19 Data Analytics - UCLA Anderson 2020 Datathon #
#########################################################

library(dplyr)
library(magrittr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(lubridate)

#' 
#' 
#' 
#' 
#' 


data <- read.csv('us-counties.csv')
data <- as_tibble(data)


#' Aggregate Calculation
#' @description Calculates descriptive statistics of COVID19 data from NY Times
#'   Git data
#' @param data Aggregate data provided by NY Times Git
#' @return Summary of CovID19 First Case, Last Case, Total Cases, Total Deaths
#'
#' @export
agg_calculations <- function(data) {
  `%>%` <- magrittr:: `%>%`
  dplyr::summarise(
    First_Case = min(date, na.rm = T), 
    Last_Case = max(date, na.rm = T), 
    Total_Cases = sum(cases, na.rm = T), 
    Total_Deaths = sum(deaths, na.rm = T)) %>%
    glimpse
}

#Overall Descriptive Statistics
data_calculations <- data %>%
  dplyr::summarise(
    First_Case = min(date, na.rm = T),
    Last_Case = max(date, na.rm = T),
    Total_Cases = sum(cases, na.rm = T),
    Total_Deaths = sum(deaths, na.rm = T)) %>%
  glimpse

#Daily cases grouped by state for each date
daily_cases <- data %>%
  group_by(date) %>%
  mutate(daily_case = dplyr::n()) %>%
  ungroup() %>%
  group_by(state) %>%
  glimpse

#Daily Cases in Washington
washington_daily_cases <- data %>%
  select(-county, -fips) %>%
  filter(state == "Washington") %>%
  group_by(date) %>%
  #group_by(state) %>%
  summarise(cum_case = sum(cases, na.rm = T),
            cum_death = sum(deaths, na.rm = T)) %>%
  glimpse

#lubridate::date(data$date)
#TO-DO: 
#1. Get Daily State by State Increase for top 5 states (cum cases, deaths)
#2. Get Monthly State by State Increase
#3. Get Daily County by County Increase
#4. Get Monthly County by County Increase
#5. Get 90 Day window period frame State by State Increase

#Get top 5 states with highest cum cases of COVID19
state_cases <- data %>%
  select(-county, -fips) %>%
  group_by(state) %>%
  summarise(total_case = sum(cases, na.rm = T),
            total_death = sum(deaths, na.rm = T)) %>%
  arrange(desc(total_case), desc(total_death)) %>%
  glimpse

#Get top 5 states with highest cum deaths of COVID19
state_death <- data %>%
  select(-county, -fips) %>%
  group_by(state) %>%
  summarise(total_case = sum(cases, na.rm = T),
            total_death = sum(deaths, na.rm = T)) %>%
  arrange(desc(total_death), desc(total_case)) %>%
  glimpse

#Plot Top 5 States with highest cum cases of COVID19
ggplot(data = head(state_cases, 5), 
       aes(x = state, y = total_case)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_case), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  geom_point()

#Plot Top 5 States with highest cum deaths of COVID19
ggplot(data = head(state_death, 5), 
       aes(x = state, y = total_death, fill = state)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_death), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  geom_point()



