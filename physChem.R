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

#### IF STARTING A NEW PROJECT READ IN THESE FILES ------------------------------------------------------------------
ProjectIDs <- read.csv('ProjectIDs.csv', 
                       colClasses = c(ProjectID_Simple = "factor", ProjectIdentifier = "character",Rank="factor"))%>%
  filter(Rank=="1"|Rank=="2")

all_wqp_PhysChemData <- read.csv('All_WQP_PhysChemData_Cleaned.csv',
                                  colClasses = c(ProjectID_Simple = "factor", ActivityStartDate ="date"))

all_wqp_PhysChemData<-all_wqp_PhysChemData %>%
  mutate(OrganizationFormalName= as.character(OrganizationFormalName),
         ResultMeasure.MeasureUnitCode=as.factor(ResultMeasure.MeasureUnitCode),
         DetectionQuantitationLimitMeasure.MeasureValue=as.numeric(DetectionQuantitationLimitMeasure.MeasureValue),
         DetectionQuantitationLimitMeasure.MeasureUnitCode=as.factor(DetectionQuantitationLimitMeasure.MeasureUnitCode),
         ActivityStartDate=as.Date(ActivityStartDate),
         ResultMeasureValue=as.numeric(ResultMeasureValue)) 

summary_df<- read.csv('Summary_PhysChemData_Cleaned.csv')

###Create a file to relate the project IDs to groups ---------------------------------------------------------------

ProjectIDs <- read.csv('ProjectIDs.csv', 
                       colClasses = c(ProjectID_Simple = "factor", ProjectIdentifier = "character",Rank="factor"))%>%
  filter(Rank=="1"|Rank=="2")

###Create a function to download narrow data by Project ID
get_project_data <- function(proj_id) {
  tryCatch({
    data <- readWQPdata(project = proj_id, dataProfile = "resultPhysChem")
    if (nrow(data) == 0) return(NULL)
    date_cols <- c("ActivityStartDate", "ActivityEndDate", "AnalysisStartDate", "AnalysisEndDate","ResultMeasureValue")
    for (col in date_cols) {
      if (col %in% names(data)) {
        data[[col]] <- as.character(data[[col]])}}
    data$ProjectIdentifier <- proj_id
    return(data)},
    error = function(e) {
      message("Error retrieving data for ", proj_id, ": ", e$message)
      return(NULL)})}

### Download the phys results for all the groups and then join into one dataframe -------------

all_wqp_PhysChemData <- map_dfr(ProjectIDs$ProjectIdentifier, get_project_data)
all_wqp_PhysChemData <- left_join(all_wqp_PhysChemData, ProjectIDs, by = "ProjectIdentifier")

### Keep only selected columns ----------------------------------------------------------------

cols_to_keep <- c(
  "ProjectIdentifier",
  "ProjectID_Simple",
  "OrganizationIdentifier",
  "OrganizationFormalName",
  "ActivityIdentifier",
  "ActivityStartDate",
  "MonitoringLocationIdentifier",
  "CharacteristicName",
  "ResultMeasureValue",
  "ResultMeasure.MeasureUnitCode",
  "DetectionQuantitationLimitMeasure.MeasureValue",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "ActivityLocation.LatitudeMeasure",
  "ActivityLocation.LongitudeMeasure",
  "MonitoringLocationName")

###Filter to only the necessary columns---------------------------------------------------------------------

all_wqp_PhysChemData <- all_wqp_PhysChemData[, intersect(cols_to_keep, names(all_wqp_PhysChemData))]

### Clean the data types in each column---------------------------------------------------------------------
all_wqp_PhysChemData<-all_wqp_PhysChemData %>%
  mutate(OrganizationFormalName= as.character(OrganizationFormalName),
         ResultMeasure.MeasureUnitCode=as.factor(ResultMeasure.MeasureUnitCode),
         DetectionQuantitationLimitMeasure.MeasureValue=as.numeric(DetectionQuantitationLimitMeasure.MeasureValue),
         DetectionQuantitationLimitMeasure.MeasureUnitCode=as.factor(DetectionQuantitationLimitMeasure.MeasureUnitCode),
         ActivityStartDate=as.Date(ActivityStartDate),
         CharacteristicName=as.factor(CharacteristicName))

### Make the NA values in results 1/2 the detection limit --------------------------------------------------

all_wqp_PhysChemData$ResultMeasureValue[is.na(all_wqp_PhysChemData$ResultMeasureValue)] <- all_wqp_PhysChemData$DetectionQuantitationLimitMeasure.MeasureValue[is.na(all_wqp_PhysChemData$ResultMeasureValue)] / 2
all_wqp_PhysChemData$ResultMeasure.MeasureUnitCode[is.na(all_wqp_PhysChemData$ResultMeasure.MeasureUnitCode)] <- all_wqp_PhysChemData$DetectionQuantitationLimitMeasure.MeasureUnitCode[is.na(all_wqp_PhysChemData$ResultMeasure.MeasureUnitCode)] 

write_csv(all_wqp_PhysChemData,'All_WQP_PhysChemData_Cleaned.csv')

summary(all_wqp_PhysChemData)

### Summarize and group the data by ProjectID_Simple -----------------------------------------------------

summary_df <- all_wqp_PhysChemData %>%
  group_by(ProjectID_Simple) %>%
  summarise(
    StartDate = min(as.Date(ActivityStartDate, format = "%Y-%m-%d"), na.rm = TRUE),
    EndDate = max(as.Date(ActivityStartDate, format = "%Y-%m-%d"), na.rm = TRUE),
    Measurements = n(),  # total rows, for each project ID
    Parameters_Sampled = n_distinct(CharacteristicName),
    n_SamplingEvents = n_distinct(ActivityIdentifier),
    n_SamplingDates = n_distinct(ActivityStartDate),
    Parameter_List = str_c(sort(unique(CharacteristicName)), collapse = ","),
    .groups = "drop"
  ) %>%
  arrange(ProjectID_Simple)

write_csv(summary_df,'Summary_PhysChemData_Cleaned.csv')

### Create a lat and long dataframe and remove duplicate rows-------------------------------------------------------

project_locations <- all_wqp_PhysChemData %>%
  select(ProjectID_Simple,
         MonitoringLocationName,
         ActivityLocation.LatitudeMeasure,
         ActivityLocation.LongitudeMeasure) %>%
         distinct() %>%   
  arrange(ProjectID_Simple, MonitoringLocationName)

projectDate_locations <- all_wqp_PhysChemData %>%
  select(ProjectID_Simple,
         MonitoringLocationName,
         ActivityLocation.LatitudeMeasure,
         ActivityLocation.LongitudeMeasure,ActivityStartDate) %>%
  distinct() %>%   
  arrange(ProjectID_Simple, MonitoringLocationName)

write_csv(project_locations,'project_locations_lat_long.csv')
write_csv(projectDate_locations,'projectDate_locations_lat_long.csv')



