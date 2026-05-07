# Setup -----------------------------------------------------------------------------------------------------------
rm(list=ls()); # remove all objects from workspace
setwd("~/Desktop/Masters/ProfessionalPaper/Rdata")
library(dataRetrieval)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(tidyverse)
library(treemapify)
library(ggplot2)

#### IF STARTING A NEW PROJECT READ IN THESE FILES ----------------------------------

PhysChemParm <- read.csv('ParmType_wqp_PhysChemData.csv',
                                 colClasses = c(ProjectID_Simple = "factor"))

#### Replace retired names with current parameter names

all_wqp_PhysChemData$CharacteristicName[all_wqp_PhysChemData$CharacteristicName == 
                                          "Nutrient-nitrogen***retired***use TOTAL NITROGEN, MIXED FORMS with speciation AS N"] <- 
  "Total Nitrogen, mixed forms"
all_wqp_PhysChemData$CharacteristicName[all_wqp_PhysChemData$CharacteristicName == 
                                          "Inorganic nitrogen (nitrate and nitrite) ***retired***use Nitrate + Nitrite"] <-
  "Nitrate + Nitrite"

#Parameter_List = str_c(sort(unique(all_wqp_PhysChemData$CharacteristicName)), collapse = ",")

### Add a column to the phys/chem data to categorize the type of parameters and named group-----------------------------------------

parm_type <- read.csv('parameterClasses.csv', 
                      colClasses = c(CharacteristicName = "character", Parameter_type = "factor", Derived="factor"))

ParmType_wqp_PhysChemData <- left_join(all_wqp_PhysChemData, parm_type, by = "CharacteristicName")

#### rename parameters that are unclear or that can be simplified 
ParmType_wqp_PhysChemData$CharacteristicName[ParmType_wqp_PhysChemData$CharacteristicName == 
                                          "Count"] <-"Macroinvertebrate Count"

ParmType_wqp_PhysChemData$CharacteristicName[ParmType_wqp_PhysChemData$CharacteristicName == 
                                               "Dissolved oxygen (DO)"] <-"Dissolved oxygen"
ParmType_wqp_PhysChemData$CharacteristicName[ParmType_wqp_PhysChemData$CharacteristicName == 
                                               "RBP Turbidity Code (choice list)"] <-"RBP Turbidity Code"
ParmType_wqp_PhysChemData$CharacteristicName[ParmType_wqp_PhysChemData$CharacteristicName == 
                                               "Chlorophyll a, corrected for pheophytin"] <-"Chlorophyll a*"
ParmType_wqp_PhysChemData$CharacteristicName[ParmType_wqp_PhysChemData$CharacteristicName == 
                                               "Depth, Secchi disk depth"] <-"Secchi disk depth"

write_csv(ParmType_wqp_PhysChemData,'ParmType_wqp_PhysChemData.csv')

#### Visualize the parameters to check classification

char_counts <-PhysChemParm  %>%
  count(CharacteristicName, Parameter_type, name = "Count") %>%
  arrange(desc(Count))

threshold <- 350

char_counts <- char_counts %>%
  mutate(CharacteristicName = if_else(
    Count < threshold,
    "All Other Parameters",
    CharacteristicName),
    Parameter_type = if_else(
      CharacteristicName == "All Other Parameters",
      "Multiple",
      Parameter_type)
  ) %>%
  group_by(CharacteristicName, Parameter_type) %>%
  summarize(Count = sum(Count), .groups = "drop")

wrap_width <- 10
char_counts <- char_counts %>%
  mutate(label_wrapped = str_wrap(CharacteristicName, width = wrap_width))

ggplot(char_counts,
       aes(area = Count,
           fill = Parameter_type,
           label = label_wrapped)) +
  geom_treemap(colour = "white",
               size = 0.4) +
  geom_treemap_text(
    fontface = "bold",
    colour = "white",
    place = "centre",
    grow = TRUE) +
  scale_fill_manual(
    values = c(
      "Biological" = "gold2",
      "Chemical"   = "#a8d5e2",
      "Physical"   = "#104911",
      "Sediment"   = "#548C2F",
      "Metals"     = "pink2",
      "Nutrients"  = "#f9a620",
      "Multiple"   = "grey60"
    )) +
  labs(fill = "Parameter Type") +
  theme_minimal()+
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12))

### Create graphics of why types of parameters are sampled, over what years, and across projects-------------------------

Parameter_type <- PhysChemParm$Parameter_type
parmCount<-ggplot(PhysChemParm, aes(x=Parameter_type, fill=Derived))+
  geom_bar()+
  scale_fill_manual(values = c("Lab"="#548C2F", "Field"="#f9a620"))+
  theme_linedraw()+
  labs(x="Water Quality Parameter Type", y= "Count")+
  ylim(0,97000)

ggsave("parameters.png", width = 6.5, height = 3.5, units = "in", dpi = 300)

build<-ggplot_build(parmCount)
build_dataframe<-build$data[[1]]
### Create a table to classify parameters

library(gt)

# Create a summary table listing characteristics under each parameter type
classification_table <- parm_type %>%
  arrange(Parameter_type, CharacteristicName) %>%
  group_by(Parameter_type) %>%
  summarise(
    Characteristics = paste(CharacteristicName, collapse = "; "),
    .groups = "drop")

# Create a styled gt table suitable for a scientific paper
gt_table<- classification_table %>%
  gt() %>%
  tab_header(
    title = md("**Classification of Water Quality Characteristics by Parameter Type**"),
    subtitle = "Used for grouping variables in subsequent figures and analyses"
  ) %>%
  cols_label(
    Parameter_type = "Parameter Type",
    Characteristics = "Included Characteristics"
  ) %>%
  opt_all_caps() %>%
  tab_source_note(
    source_note = md("Parameter classifications were developed to support figure interpretation and analytical grouping.")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(table.font.size = 12,
    data_row.padding = px(6))

gtsave( gt_table,filename = "Table_S1_Parameter_Classification.docx")

library(webshot2)
gtsave(gt_table,filename = "Table_S1_Parameter_Classification.pdf")

##### Create a dataframe listing what parameters each groups is measuring
parmsbyGroup<- ParmType_wqp_PhysChemData %>%
  select(ProjectID_Simple,Parameter_type)%>%
  distinct()
write_csv(parmsbyGroup,'parmsbyGroup.csv')


#--------------------
#OLD TREEMAP
#--------------------
char_counts <- ParmType_wqp_PhysChemData %>% 
  count(CharacteristicName, name = "Count") %>% 
  arrange(desc(Count)) 

threshold <- 250 

char_counts <- char_counts %>% 
  mutate(CharacteristicName = if_else( Count < threshold, "All Other Parameters", CharacteristicName)) %>% 
  group_by(CharacteristicName) %>% 
  summarize(Count = sum(Count), 
            .groups = "drop") 

wrap_width <- 10 
char_counts <- char_counts %>% 
  mutate(label_wrapped = str_wrap(CharacteristicName, width = wrap_width)) 

ggplot(char_counts, 
       aes(area = Count, fill = Count, label = label_wrapped)) + 
  geom_treemap() + 
  geom_treemap_text( 
    fontface = "bold", colour = "white", place = "centre", grow = TRUE) + 
  scale_fill_viridis_c(option = "turbo") + 
  labs(fill = "Sample Count") + theme_minimal()