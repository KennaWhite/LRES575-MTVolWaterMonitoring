library(tidyverse)
library(lubridate)

#-----------------------------
# 1. Read in data and create summary
#-----------------------------
parms_by_goal <- read_csv("parmsbyGoals.csv")

PhysChemParm<- read_csv("ParmType_wqp_PhysChemData.csv") %>%
  mutate(
    ActivityStartDate = as.Date(ActivityStartDate),
    sample_month = floor_date(ActivityStartDate, "month"),
    sample_year  = year(ActivityStartDate))
#----------------------------- Summarize data
physchem_summary <- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(
    dataCount = n(),
    
    first_year = min(sample_year, na.rm = TRUE),
    last_year  = max(sample_year, na.rm = TRUE),
    
    # Inclusive number of years spanned
    dataRange_yrs = (last_year - first_year) + 1,
    
    # Convert to months for weighting
    total_months_spanned = dataRange_yrs * 12,
    
    # Number of months with sampling activity
    months_sampled = n_distinct(sample_month),
    
    # Effort-weighted data count
    weightedDataCount =
      (dataCount * months_sampled) / total_months_spanned,
    .groups = "drop")
### Join into new dataframe
groups_goals_parms <- parms_by_goal %>%
  left_join(
    physchem_summary,
    by = c("ProjectID_Simple", "Parameter_type"))
write_csv(groups_goals_parms,'groups_goals_parms.csv')

#-----------------------------
# 2. Graph by goal type and parameter
#-----------------------------
#look at data over time
Baseline <- groups_goals_parms %>%
  filter(Goal_baseline==1)
Trend <- groups_goals_parms %>%
  filter(Goal_trend==1)
Source <-groups_goals_parms %>%
  filter(Goal_source==1)
Effective <-groups_goals_parms %>%
  filter(Goal_effective==1)
Education <-groups_goals_parms %>%
  filter(Goal_education==1)

ggplot(data = Baseline, aes(x = dataRange_yrs, y = weightedDataCount, color=Parameter_type)) +
geom_point()+
  scale_y_log10()

ggplot(data = Baseline, aes(x = factor(Parameter_type), y = weightedDataCount)) +
  geom_boxplot()+
  geom_point()+
  scale_y_log10()+
  theme_bw()

### look at weighted data
ggplot(data = filter(Baseline,Parameter_type=="Physical"), aes(x = dataRange_yrs, y = weightedDataCount, color=Parameter_type)) +
  geom_point()+
  scale_y_log10()

