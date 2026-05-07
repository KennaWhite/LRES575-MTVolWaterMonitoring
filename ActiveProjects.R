# Setup -----------------------------------------------------------------------------------------------------------
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
setwd("~/Desktop/Masters/ProfessionalPaper/Rdata")
library(dataRetrieval)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(tidyverse)
library(treemapify)

#### IF STARTING A NEW PROJECT READ IN THESE FILES ----------------------------------

all_wqp_PhysChemData <- read.csv('All_WQP_PhysChemData_Cleaned.csv',
                                 colClasses = c(ProjectID_Simple = "factor"))


#### To graph a 1-year period use this code ----------------------------------------
project_years <- all_wqp_PhysChemData %>%
  mutate(ActivityStartDate = parse_date_time(ActivityStartDate,
                                             orders = c("ymd", "mdy", "dmy")))
project_years <- project_years %>%
  group_by(ProjectID_Simple) %>%
  summarize(
    start = min(ActivityStartDate, na.rm = TRUE),
    end = max(ActivityStartDate, na.rm = TRUE),
    .groups = "drop")

start_year <- 2000
end_year   <- 2025

project_yearBars <- project_years %>%
  mutate(
    start_y = year(start),
    end_y   = year(end)
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(start_y, end_y))) %>%
  unnest(year) %>%
  filter(year >= start_year, year <= end_year)

project_yearBars <- project_yearBars %>%
  mutate(bin = cut(year,
                   breaks = seq(start_year, end_year, by = 1),
                   right = FALSE,
                   include.lowest = TRUE,
                   labels = paste0(seq(start_year, end_year - 1, by = 1))))
bin_counts <- project_yearBars %>%
  distinct(ProjectID_Simple, bin) %>%      # keep only one row per project per bin
  count(bin, name = "Number_of_Projects")

ggplot(bin_counts, aes(x = bin, y = Number_of_Projects)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Number of Projects Active per Year",
    x = "1-Year Period",
    y = "Number of Active Projects"
  ) +
  theme_bw()+
theme(plot.title = element_text(hjust = 0.5))

ggsave("activeProjects.png", width = 9, height = 4, units = "in")


#### To graph a 5-year period use this code ----------------------------------------

project_years <- all_wqp_PhysChemData %>%
  mutate(ActivityStartDate = parse_date_time(ActivityStartDate,
                                             orders = c("ymd", "mdy", "dmy")))
project_years <- project_years %>%
  group_by(ProjectID_Simple) %>%
  summarize(
    start = min(ActivityStartDate, na.rm = TRUE),
    end = max(ActivityStartDate, na.rm = TRUE),
    .groups = "drop")

start_year <- 2000
end_year   <- 2025

project_yearBars <- project_years %>%
  mutate(
    start_y = year(start),
    end_y   = year(end)
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(start_y, end_y))) %>%
  unnest(year) %>%
  filter(year >= start_year, year <= end_year)

project_yearBars <- project_yearBars %>%
  mutate(bin = cut(year,
                   breaks = seq(start_year, end_year, by = 5),
                   right = FALSE,
                   include.lowest = TRUE,
                   labels = paste0(seq(start_year, end_year - 5, by = 5),
                                   "–",
                                   seq(start_year + 4, end_year - 1, by = 5))))
bin_counts <- project_yearBars %>%
  distinct(ProjectID_Simple, bin) %>%      # keep only one row per project per bin
  count(bin, name = "Number_of_Projects")

ggplot(bin_counts, aes(x = bin, y = Number_of_Projects)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Number of Projects Active per 5-Year Period",
    x = "5-Year Period",
    y = "Number of Active Projects"
  ) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("activeProjects.png", width = 9, height = 4, units = "in")