# Setup -----------------------------------------------------------------------------------------------------------
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
setwd("~/Desktop/Masters/ProfessionalPaper/Rdata")
library(dataRetrieval)
library(dplyr)
library(readr)
library(purrr)
library(tidyverse)
library(treemapify)

#### IF STARTING A NEW PROJECT READ IN THESE FILES ----------------------------------
ProjectNames <- read.csv('ProjectNames.csv', 
                       colClasses = c(ProjectID_Simple = "factor", ProjectName = "character",OrganizationName="character", ganttOrder="numeric"))

all_wqp_PhysChemData <- read.csv('All_WQP_PhysChemData_Cleaned.csv',
                                      colClasses = c(ProjectID_Simple = "factor", ActivityStartDate ="date"))

##### Create a gantt style chart with the groups in order by start date, with similar groups positioned next to each other

all_wqp_PhysChemData <- all_wqp_PhysChemData %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate))
project_dates <- all_wqp_PhysChemData %>%
  distinct(ProjectID_Simple, ActivityStartDate)
project_dates <- left_join(project_dates, ProjectNames,
                           by = "ProjectID_Simple")

### Create the project order to appear in the chart 

project_order <- project_dates %>%
  group_by(ProjectID_Simple, ProjectName) %>%
  summarize(first_order = min(ganttOrder), .groups = "drop") %>%
  arrange(first_order)
project_dates <- project_dates %>%
  mutate(ProjectID_Simple = factor(
    ProjectID_Simple,
    levels = project_order$ProjectID_Simple))

### Create the plot ----- y-labs have project name

ggplot(project_dates,
       aes(x = ActivityStartDate, y = ProjectID_Simple)) +
  geom_point(color = "seagreen", size = 2, alpha = 1) +
  scale_y_discrete(labels = str_wrap(project_order$ProjectName,width=55),
                   ) +
  scale_x_date(limits = c(ymd("2000-01-01"), ymd("2027-01-01")),
               date_breaks = "3 years",
               date_labels = "%Y") +
  labs(
    title = "Sampling Dates by Monitoring Group Project",
    x = "Year",
    y = "Monitoring Group Project") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("my_plot.png", width = 11, height = 13, units = "in")

-----------------------------------------------------------------------------------
# Code to make the Gantt chart appear as bars not points #
------------------------------------------------------------------------------------
all_wqp_PhysChemData <- all_wqp_PhysChemData %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate))

project_range <- all_wqp_PhysChemData %>%
  group_by(ProjectID_Simple) %>%
  summarize(
    start_date = min(ActivityStartDate, na.rm = TRUE),
    end_date   = max(ActivityStartDate, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(start_date)
project_range <- project_range %>%
  mutate(ProjectID_Simple = factor(ProjectID_Simple, levels = ProjectID_Simple))

ggplot(project_range,
       aes(x = ActivityStartDate, y = ProjectID_Simple)) +
geom_segment(aes(x = start_date, xend = end_date,
y = ProjectID_Simple, yend = ProjectID_Simple),
linewidth = 2, color="seagreen")+
scale_y_discrete(labels = str_wrap(project_order$ProjectName,width=55),
) +
scale_x_date(limits = c(ymd("2000-01-01"), ymd("2027-01-01")),
             date_breaks = "3 years",
             date_labels = "%Y") +
labs(
  title = "Sampling Dates by Monitoring Group Project",
  x = "Year",
  y = "Monitoring Group Project") +
theme_bw()+
theme(plot.title = element_text(hjust = 0.5))
