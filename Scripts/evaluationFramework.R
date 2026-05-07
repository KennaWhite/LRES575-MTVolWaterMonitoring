library(tidyverse)
library(ggh4x)
library(basetheme)

#-----------------------------
# 1. Read in data and create a summary
#-----------------------------
parms_by_goal <- read_csv("parmsbyGoals.csv")
ProjectNames <-read_csv("ProjectNames.csv")

parms_by_goal <- left_join(parms_by_goal, ProjectNames, by = c("ProjectID_Simple"))

PhysChemParm<- read_csv("ParmType_wqp_PhysChemData.csv") %>%
  mutate(
    ActivityStartDate = as.Date(ActivityStartDate),
    sample_month = floor_date(ActivityStartDate, "month"),
    sample_year  = year(ActivityStartDate))

#-----------------------------
# Summarize the data
#-----------------------------

physchem_summary <- PhysChemParm %>%
  group_by(ProjectID_Simple,Parameter_type) %>%
  summarise(
    dataCount = n(),
    
    first_year = min(sample_year, na.rm = TRUE),
    last_year  = max(sample_year, na.rm = TRUE),
    
    # Inclusive number of years spanned
    dataRange_yrs = (last_year - first_year) + 1,
    
    # Number of months with sampling activity
    months_sampled = n_distinct(sample_month),
    .groups = "drop")

dataRange_yrs<-physchem_summary %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(dataRange_yrs,
    .groups = "drop")

### Data point summary
datapoints_summary <- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(dataCount = n(),
    .groups = "drop")

### Duration of sampling

months_sampled_summary <- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(
    months_sampled = n_distinct(sample_month),
    .groups = "drop")

#### Calculating the median number analytes per year

analytes_per_year <- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type, sample_year) %>%
  summarise(
    analytes = n_distinct(CharacteristicName),
    .groups = "drop"
  ) %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(
    median_analytes_per_year = median(analytes),
    .groups = "drop")

### Median number of sites per year

sites_per_year <- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type, sample_year) %>%
  summarise(
    sites = n_distinct(MonitoringLocationIdentifier),
    .groups = "drop") %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(
    median_sites_per_year = median(sites),
    .groups = "drop")

### Number of Unique Sampling Activities per year

sampling_days_year<- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type, sample_year) %>%
  summarise(
    activity=n_distinct(ActivityStartDate),
    .groups = "drop") %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(
    median_sampling_days_year = median(activity),
    .groups = "drop")

### Months monitored per year

months_per_year <- PhysChemParm %>%
  group_by(ProjectID_Simple, Parameter_type, sample_year) %>%
  summarise(
    months_monitored = n_distinct(sample_month),
    .groups = "drop") %>%
  group_by(ProjectID_Simple, Parameter_type) %>%
  summarise(
    median_months_per_year = median(months_monitored),
    .groups = "drop")

#------------------------------------
#Combine into plotting dataframe
#------------------------------------

goalEvaluationDf<-parms_by_goal %>%
  left_join(datapoints_summary,
           by = c("ProjectID_Simple", "Parameter_type")) %>%
  left_join(dataRange_yrs,
            by = c("ProjectID_Simple", "Parameter_type")) %>%
  left_join(analytes_per_year,
            by = c("ProjectID_Simple", "Parameter_type")) %>%
  left_join(sites_per_year,
            by = c("ProjectID_Simple", "Parameter_type")) %>%
  left_join(sampling_days_year,
            by = c("ProjectID_Simple", "Parameter_type")) %>%
  # left_join(spatial_intensity,
  #           by = c("ProjectID_Simple", "Parameter_type", "median_datapoints_per_site_year","median_sites_per_year")) %>%
  left_join(months_per_year,
            by = c("ProjectID_Simple", "Parameter_type"))

write_csv(goalEvaluationDf,'goalEvaluationDf.csv')

#-------------------------------------
# 2. Graph by goal type and parameter
#-------------------------------------
#Filter the data to goal type
Baseline <- goalEvaluationDf %>%
  filter(Goal_baseline==1)%>%
  group_by(ProjectID_Simple, Parameter_type)
Trend <- goalEvaluationDf  %>%
  filter(Goal_trend==1)
Source <-goalEvaluationDf  %>%
  filter(Goal_source==1)
Effective <-goalEvaluationDf  %>%
  filter(Goal_effective==1)
Education <-goalEvaluationDf  %>%
  filter(Goal_education==1)

### Seting up for side by side plots

plot_data_effective <- filter(goalEvaluationDf,Goal_effective==1) %>% #replace Goal_ with type wanted
  select(
    ProjectID_Simple,
    ProjectNameShort,
    Parameter_type,
    dataCount,
    dataRange_yrs,
    median_analytes_per_year,
    median_sites_per_year,
    median_sampling_days_year,
    median_months_per_year ) %>%
  pivot_longer(
    cols = -c(ProjectID_Simple, ProjectNameShort,Parameter_type),
    names_to = "metric",
    values_to = "value") %>%
  mutate(
    metric = factor(metric,
      levels = c(
        "dataCount",
        "dataRange_yrs",
        "median_analytes_per_year",
        "median_sites_per_year",
        "median_sampling_days_year",
        "median_months_per_year" ),
      labels = c(
        "Number of Data Points",
        "Duration of Sampling (Years)",
        "Analytes Measured per Year",
        "Sites Monitored per Year",
        "Unique Sampling Events per Year",
        "Months Monitored per Year")))

### Setting up the groups I want to call out

highlight_projects <- c(
  "Big Hole WC Elkhorn",
  "Gallatin TF Nut",
  "Clearwater Nut",
  "Big Horn River",
  "Madison",
  "Sun",
  "NWM Lakes 1",
  "Whitefish LI",
  "Stillwater Rosebud",
  "Bitterroot Sapphire")

effective_projects <- c(
  "Gallatin TF VM",
  "Broadwater Deep Crk")

plot_data_highlight <-plot_data_baseline %>% #replace plot_data_XXXX with goal type wanted
  mutate(
    highlight = ifelse(
      ProjectNameShort %in% highlight_projects,
      ProjectNameShort,
      "Other projects"))


#-----------------------------
### PLOTTING BASLINE
#-----------------------------
param_counts_baseline <- plot_data_highlight %>% #replace param_counts_XXX with goal type wanted
  distinct(ProjectID_Simple, Parameter_type) %>%
  count(Parameter_type, name = "n")
param_labels <- param_counts_baseline %>%
  mutate(
    label = paste0(Parameter_type, " (n = ", n, ")")
  ) %>%
  select(Parameter_type, label)

param_label_vector <- setNames(
  param_labels$label,
  param_labels$Parameter_type)

ggplot(plot_data_highlight,aes(x = Parameter_type, y = value)) +
  geom_boxplot(outlier.size = 0.4)+
  geom_point(aes(color=highlight, size = highlight != "Other projects"))+
  scale_size_manual(values = c(`TRUE` = 2, `FALSE` = 0.5), guide = "none")+
  facet_wrap(~ metric, scales = "free_y",ncol = 2) +
  facetted_pos_scales(y = list(metric == "Number of Data Points" ~ scale_y_log10(),
                               metric=="Analytes Measured per Year"~scale_y_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2)),
                               metric=="Duration of Sampling (Years)"~scale_y_continuous(limits = c(0, 25),breaks = seq(0, 25, by = 5)),
                               metric=="Sites Monitored per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20)),
                               metric=="Months Monitored per Year"~scale_y_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 6)),
                               metric=="Unique Sampling Events per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20))))+
  scale_x_discrete(labels = param_label_vector) +
  scale_color_manual(
    values = c(
      "Other projects" = "black",
      "Big Hole WC Elkhorn" = "#D55E00",
      "Gallatin TF Nut" = "#0072B2",
      "Clearwater Nut" = "#009E73",
      "Big Horn River" = "#CC79A7",
      "Madison" = "lightblue1",
      "Sun" = "#F0E442",
     "NWM Lakes 1" = "#E69F00",
     "Whitefish LI" = "#548C2F",
     "Stillwater Rosebud" = "#332288",
      "Bitterroot Sapphire" = "green"),
    breaks = highlight_projects,
    name = "Highlighted Projects") +
  labs(
    title = NULL,
    y = NULL,
    x=NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=10),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size=10),
    strip.text = element_text(face = "bold",size=10),
    legend.position = "right",
    panel.grid.minor = element_blank())

### PLOTTING TREND
param_counts_trend <- plot_data_highlight %>% #replace param_counts_XXX with goal type wanted
  distinct(ProjectID_Simple, Parameter_type) %>%
  count(Parameter_type, name = "n")
param_labels <- param_counts_trend %>%
  mutate(
    label = paste0(Parameter_type, " (n = ", n, ")")
  ) %>%
  select(Parameter_type, label)

param_label_vector <- setNames(
  param_labels$label,
  param_labels$Parameter_type)
plot_data_highlightT <-plot_data_trend %>% #replace plot_data_XXXX with goal type wanted
  mutate(
    highlight = ifelse(
      ProjectNameShort %in% highlight_projects,
      ProjectNameShort,
      "Other projects"))

ggplot(plot_data_highlightT,aes(x = Parameter_type, y = value)) +
  geom_boxplot(outlier.size = 0.4)+
  geom_point(aes(color=highlight, size = highlight != "Other projects"))+
  scale_size_manual(values = c(`TRUE` = 2, `FALSE` = 0.75), guide = "none")+
  facet_wrap(~ metric, scales = "free_y",ncol = 2) +
  facetted_pos_scales(y = list(metric == "Number of Data Points" ~ scale_y_log10(),
                               metric=="Analytes Measured per Year"~scale_y_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2)),
                               metric=="Duration of Sampling (Years)"~scale_y_continuous(limits = c(0, 25),breaks = seq(0, 25, by = 5)),
                               metric=="Sites Monitored per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20)),
                               metric=="Months Monitored per Year"~scale_y_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 6)),
                               metric=="Unique Sampling Events per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20))))+
  scale_x_discrete(labels = param_label_vector) +
  scale_color_manual(
    values = c(
      "Other projects" = "black",
      "Big Hole WC Elkhorn" = "#D55E00",
      "Gallatin TF Nut" = "#0072B2",
      "Clearwater Nut" = "#009E73",
      "Big Horn River" = "#CC79A7",
      "Madison" = "lightblue1",
      "Sun" = "#F0E442",
      "NWM Lakes 1" = "#E69F00",
      "Whitefish LI" = "#548C2F",
      "Stillwater Rosebud" = "#332288",
      "Bitterroot Sapphire" = "green"),
    breaks = highlight_projects,
    name = "Highlighted Projects") +
  labs(
    title = NULL,
    y = NULL,
    x=NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=10),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size=10),
    strip.text = element_text(face = "bold",size=10),
    legend.position = "right",
    panel.grid.minor = element_blank())

#-----------------------------
### PLOTTING SOURCE
#-----------------------------

plot_data_highlightS <-plot_data_source %>% #replace plot_data_XXXX with goal type wanted
  mutate(
    highlight = ifelse(
      ProjectNameShort %in% highlight_projects,
      ProjectNameShort,
      "Other projects"))

Param_counts_source <- plot_data_highlightS %>% #replace param_counts_XXX with goal type wanted
  distinct(ProjectID_Simple, Parameter_type) %>%
  count(Parameter_type, name = "n")
param_labels <- Param_counts_source %>%
  mutate(
    label = paste0(Parameter_type, " (n = ", n, ")")
  ) %>%
  select(Parameter_type, label)

param_label_vector <- setNames(
  param_labels$label,
  param_labels$Parameter_type)

ggplot(plot_data_highlightS,aes(x = Parameter_type, y = value)) +
  geom_boxplot(outlier.size = 0.4)+
  geom_point(aes(color=highlight, size = highlight != "Other projects"))+
  scale_size_manual(values = c(`TRUE` = 2, `FALSE` = 0.5), guide = "none")+
  facet_wrap(~ metric, scales = "free_y",ncol = 2) +
  facetted_pos_scales(y = list(metric == "Number of Data Points" ~ scale_y_log10(),
                               metric=="Analytes Measured per Year"~scale_y_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2)),
                               metric=="Duration of Sampling (Years)"~scale_y_continuous(limits = c(0, 25),breaks = seq(0, 25, by = 5)),
                               metric=="Months Monitored per Year"~scale_y_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 6)),
                               metric=="Sites Monitored per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20)),
                               metric=="Unique Sampling Events per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20))))+
  scale_x_discrete(labels = param_label_vector) +
  scale_color_manual(
    values = c(
      "Other projects" = "black",
      "Big Hole WC Elkhorn" = "#D55E00",
      "Gallatin TF Nut" = "#0072B2",
      "Clearwater Nut" = "#009E73",
      "Big Horn River" = "#CC79A7",
      "Madison" = "lightblue1",
      "Sun" = "#F0E442",
      "NWM Lakes 1" = "#E69F00",
      "Whitefish LI" = "#548C2F",
      "Stillwater Rosebud" = "#332288",
      "Bitterroot Sapphire" = "green"),
    breaks = highlight_projects,
    name = "Highlighted Projects") +
  labs(
    title = NULL,
    y = NULL,
    x=NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=10),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size=10),
    strip.text = element_text(face = "bold",size=10),
    legend.position = "right",
    panel.grid.minor = element_blank())

#-----------------------------
### PLOTTING EDUCATION
#-----------------------------
plot_data_highlightEd <-plot_data_education %>% #replace plot_data_XXXX with goal type wanted
  mutate(
    highlight = ifelse(
      ProjectNameShort %in% highlight_projects,
      ProjectNameShort,
      "Other projects"))

Param_counts_education <- plot_data_highlightEd %>% #replace param_counts_XXX with goal type wanted
  distinct(ProjectID_Simple, Parameter_type) %>%
  count(Parameter_type, name = "n")
param_labels <- Param_counts_education %>%
  mutate(
    label = paste0(Parameter_type, " (n = ", n, ")")
  ) %>%
  select(Parameter_type, label)

param_label_vector <- setNames(
  param_labels$label,
  param_labels$Parameter_type)

ggplot(plot_data_highlightEd,aes(x = Parameter_type, y = value)) +
  geom_boxplot(outlier.size = 0.4)+
  geom_point(aes(color=highlight, size = highlight != "Other projects"))+
  scale_size_manual(values = c(`TRUE` = 2, `FALSE` = 0.5), guide = "none")+
  facet_wrap(~ metric, scales = "free_y",ncol = 2) +
  facetted_pos_scales(y = list(metric == "Number of Data Points" ~ scale_y_log10(),
                               metric=="Analytes Measured per Year"~scale_y_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2)),
                               metric=="Months Monitored per Year"~scale_y_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 6)),
                               metric=="Duration of Sampling (Years)"~scale_y_continuous(limits = c(0, 25),breaks = seq(0, 25, by = 5)),
                               metric=="Sites Monitored per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20)),
                               metric=="Unique Sampling Events per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20))))+
  scale_x_discrete(labels = param_label_vector) +
   scale_color_manual(
    values = c(
      "Other projects" = "black",
      "Big Hole WC Elkhorn" = "#D55E00",
      "Gallatin TF Nut" = "#0072B2",
      "Clearwater Nut" = "#009E73",
      "Big Horn River" = "#CC79A7",
      "Madison" = "lightblue1",
      "Sun" = "#F0E442",
      "NWM Lakes 1" = "#E69F00",
      "Whitefish LI" = "#548C2F",
      "Stillwater Rosebud" = "#332288",
      "Bitterroot Sapphire" = "green"),
    breaks = highlight_projects,
    name = "Highlighted Projects") +
  labs(
    title = NULL,
    y = NULL,
    x=NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=10),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size=10),
    strip.text = element_text(face = "bold",size=10),
    legend.position = "right",
    panel.grid.minor = element_blank())

#-----------------------------
### PLOTTING EFFECTIVENESS
#-----------------------------
plot_data_highlightEf <-plot_data_effective %>% #replace plot_data_XXXX with goal type wanted
  mutate(
    highlight = ifelse(
      ProjectNameShort %in% effective_projects,
      ProjectNameShort,
      "Other projects"))

Param_counts_effective <- plot_data_highlightEf %>% #replace param_counts_XXX with goal type wanted
  distinct(ProjectID_Simple, Parameter_type) %>%
  count(Parameter_type, name = "n")
param_labels <- Param_counts_effective %>%
  mutate(
    label = paste0(Parameter_type, " (n = ", n, ")")
  ) %>%
  select(Parameter_type, label)

param_label_vector <- setNames(
  param_labels$label,
  param_labels$Parameter_type)


ggplot(plot_data_highlightEf,aes(x = Parameter_type, y = value)) +
  #geom_boxplot(outlier.size = 0.4)+
  geom_point(aes(color=highlight, size = highlight != "Other projects"))+
  scale_size_manual(values = c(`TRUE` = 2, `FALSE` = 0.5), guide = "none")+
  facet_wrap(~ metric, scales = "free_y",ncol = 2) +
  facetted_pos_scales(y = list(metric == "Number of Data Points" ~ scale_y_log10(limits = c(1, 1000)), 
                               metric=="Analytes Measured per Year"~scale_y_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2)),
                               metric=="Sites Monitored per Year"~scale_y_continuous(limits = c(0, 25),breaks = seq(0, 25, by = 5)),
                               metric=="Months Monitored per Year"~scale_y_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 6)),
                               metric=="Duration of Sampling (Years)"~scale_y_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2)),
                               metric=="Unique Sampling Events per Year"~scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20))))+
  scale_x_discrete(labels = param_label_vector) +
  scale_color_manual(
    values = c(
      "Other projects" = "black",
      "Broadwater Deep Crk" = "purple",
      "Gallatin TF VM" = "gold"),
    breaks = effective_projects,
    name = "Highlighted Projects") +
  labs(
    title = NULL,
    y = NULL,
    x=NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=10.5),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size=10),
    strip.text = element_text(face = "bold",size=10),
    legend.position = "right",
    panel.grid.minor = element_blank())
