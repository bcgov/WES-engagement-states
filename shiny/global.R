library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(networkD3)
library(ggiraph)

# -------------- Set up the migration data ---------------
migration_data_mock <- read.csv("data/migration_data.csv")

#--------------- Set up the engagement data ---------------
eng_state_data <- read.csv("data/eng_states_data.csv")
eng_state_data$Engagement.State <- factor(
  eng_state_data$Engagement.State, 
  levels = c("Engaged", "Moderately Engaged",
             "Happily Detached", "Unhappily Dedicated",
             "Minimally Engaged", "Disengaged")
)
eng_state_data <- eng_state_data %>%
  group_by(ORGID) %>%
  mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
  ungroup() %>%
  arrange(ORG, Engagement.State)

# Extract a mapping with all organization ids -> names
org_names_list <-
  c("All" = "all",
    setNames(
      eng_state_data$ORGID %>% unique() %>% as.character(),
      eng_state_data$ORG %>% unique() %>% as.character()
    ))

#--------------- Set up the data for the RMD file ---------------
library(readr)
finalMockData <- read_csv("data/finalMockData.csv", 
                          col_types = cols(COMMITMENT13 = col_number(), 
                                           COMMITMENT15 = col_number(), SAT13 = col_number(), 
                                           SAT15 = col_number(), id = col_number()))

# Start the process to turn the data into what is needed to create a combined dataset

## Turn "Wide" data (where time is delineated by different variables) into "Long" data (where there is a variable
### for time called 'Year') 

longData <- finalMockData %>% select(ORGID13,ORGANIZATION13,ORGID15,ORGANIZATION15, ENGSTATE13, ENGSTATE15) %>% 
  gather(Year, EngagementScore, ENGSTATE13:ENGSTATE15)

longData <- longData %>% mutate(Yr = ifelse(Year == "ENGSTATE13", "2013", "2015" ))

## Turn engagement score into a factor variable and recode to text labels

longData <- longData %>% mutate(EngagementScore = ifelse(Yr=="2013" & !is.na(ORGID13) & is.na(EngagementScore), "Non Respondents",
                                                         ifelse(Yr=="2015" & !is.na(ORGID15) & is.na(EngagementScore), "Non Respondents", EngagementScore)))

longData$EngagementScore <- factor(longData$EngagementScore)
levels(longData$EngagementScore) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged", "Non Respondents")

## Loop to generate engagement state tables for all ministries and then save as rds in the data folder:

for (i in unique(longData$ORGID15))
{
  assign(paste(i), longData %>% 
           filter(ORGID15==i | ORGID13==i) %>% 
           group_by(EngagementScore) %>% 
           summarize(Count2013 = sum(Yr=="2013" & ORGID13==i & !is.na(EngagementScore)), 
                     Count2015 = sum(Yr=="2015" & ORGID15==i & !is.na(EngagementScore))) %>% 
           mutate(NetGainOrLoss = Count2015 - Count2013) %>% 
           mutate(PercentageChange = ((Count2015-Count2013)/(Count2013))* 100))
}

## Combine all the organization state tables very inelegantly

### Uncomment this when plugging in real data, and comment the proceeding block of code instead

# combinedStates <- bind_rows(AG=AG,AGRI=AGRI,ARR=ARR,AVED=AVED,BCPSA=BCPSA,
#                             CFD=CFD,CSCD=CSCD,EAO=EAO,EBC=EBC,EDUC=EDUC,EM=EM,
#                             EMBC=EMBC,ENV=ENV,FIN=FIN,FLNR=FLNR,GCPE=GCPE,HLTH=HLTH,
#                             JTSTL=JTSTL,MIT=MIT,NGD=NGD,OMB=OMB,PGT=PGT,PO=PO,
#                             PSSG=PSSG,SBRT=SBRT,SDSI=SDSI,TICS=TICS,TRAN=TRAN, .id="source")

combinedStates <- bind_rows(AG=AG,AGRI=AGRI,BCPSA=BCPSA,
                            EBC=EBC,ENV=ENV,FIN=FIN,FLNR=FLNR,GCPE=GCPE,NGD=NGD,
                            PGT=PGT,PO=PO,PSSG=PSSG,SDSI=SDSI,TICS=TICS, .id="source")

### Clean up the table, and append organizational names with the tables

combinedStates <- combinedStates %>% filter(!is.na(EngagementScore))
combinedStates <- rename(combinedStates, ORGID15 = source)

OrgNameID <- longData %>% filter(!is.na(ORGID15)) %>% group_by(ORGID15,ORGANIZATION15) %>% summarise()

combinedStates <- inner_join(combinedStates, OrgNameID %>% select(ORGANIZATION15, ORGID15), by = "ORGID15")
combinedStates <- combinedStates %>% mutate(PercentageChange = round(PercentageChange,digits=2))

### Save the table as a seperate rds file, so that the rmd file can access the data

saveRDS(combinedStates,"data/combinedStates.rds")