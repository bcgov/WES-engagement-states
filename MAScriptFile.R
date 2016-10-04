MainTable1 <- as.data.frame(read.csv("data/sampledata.csv"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

# Taking into account the split of workers between TICs and Finance since 2013
MainTable3 <- mutate(MainTable1, ORGID13 = ifelse(!is.na(ORGID13_new), ORGID13_new, ORGID13), 
                     ORGANIZATION13 = ifelse(!is.na(ORGANIZATION13_new), 
                                             ORGANIZATION13_new, ORGANIZATION13))

OrgNameID <- MainTable3 %>% count(ORGANIZATION15, ORGID15)


# Renaming 2013 ORGIDs to match with 2015 ORGID formats for the same organizations 
# "Energy and Mines" is ORGID13 == "MEM" and ORGID15 == "EM"
# "International Trade" is ORGID13 == "IT" and ORGID15 == "MIT"
# "Office of the Premier" is ORGID13 =="PREM" and ORGID15 == "PO"
# "JTST" is ORGID13 == "JTST" and ORGID15 == "JTSTL"

MainTable3 <- MainTable3 %>% 
  mutate(ORGID13_15format = 
           ifelse(ORGID13 == "MEM", "EM", 
                  ifelse(ORGID13 == "IT", "MIT", 
  ifelse(ORGID13 == "PREM", "PO", 
         ifelse(ORGID13 == "JTST", "JTSTL", ORGID13)))))

# So OIPC, which was included in 2013, was out of sample in 2015; how should they be treated? As 
 # outside of public service or just completely disregarded? 

# Now creating new migration variables for MainTable3 (treating OIPC as a regular organization here)
MainTable3 <- MainTable3 %>% mutate(NR15 = (ORGID15!=" " & is.na(EngState15_Mig))) # Non-respondents in 2015
MainTable3 <- MainTable3 %>% mutate(NR13 = (ORGID13_15format!=" " & is.na(EngState13_Mig))) # Non-respondents in 2013
MainTable3 <- MainTable3 %>% mutate(NPS15 = (ORGID15!=" " & ORGID13_15format==" ")) # New to public service in 2015
MainTable3 <- MainTable3 %>% mutate(LPS13 = (ORGID13_15format!=" " & ORGID15==" ")) # Left public service since 2013
MainTable3 <- MainTable3 %>% mutate(MWPS = (ORGID13_15format!=ORGID15 & ORGID13_15format!=" " & ORGID15!=" ")) # Moved within P.S.
MainTable3$EngState13 <- recode(MainTable3$EngState13_Mig,"NA=99") #recode to save potential future headache
MainTable3$EngState15 <- recode(MainTable3$EngState15_Mig,"NA=99") #recode to save potential future headache


###
# Output into a tally table, to be used for the Sankey plot
CountOutputTable1 <- MainTable3 %>% 
  group_by(ORGID13_15format, ORGID15, EngState13_Mig, EngState15_Mig) %>% tally %>% print(n=nrow(.))

# Convert the currently Integer engagement score variables into factor variables
CountOutputTable1$EngState13_Mig <- factor(CountOutputTable1$EngState13_Mig)
levels(CountOutputTable1$EngState13_Mig) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged")
CountOutputTable1$EngState15_Mig <- factor(CountOutputTable1$EngState15_Mig)
levels(CountOutputTable1$EngState15_Mig) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged")


###
# Turn "Wide" data (where time is delineated by different variables) into "Long" data (where there is a variable
 # for time called 'Year') --- Utilize tidyr package for this, which has similar syntax as dplyr
long_MT3 <- MainTable3 %>% gather(Year, EngagementScore, EngState13:EngState15)
long_MT3 <- long_MT3 %>% mutate(Yr = ifelse(Year == "EngState13", "2013", "2015" ))

# Turn engagement score into factor variable and recode to text labels
long_MT3$EngagementScore <- factor(long_MT3$EngagementScore)
levels(long_MT3$EngagementScore) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged", "Non Respondents")


###
# Loop to generate engagement state tables for all ministries and then save as rds in the data folder:


for (i in unique(long_MT3$ORGID15))
{
  #assign(paste("StateTable",i,sep="")  
assign(paste(i), long_MT3 %>% filter(ORGID15==i | ORGID13_15format==i) %>% 
  group_by(EngagementScore) %>% 
  summarize(Count2013 = sum(Yr=="2013" & ORGID13_15format==i & !is.na(EngagementScore)), 
            Count2015 = sum(Yr=="2015" & ORGID15==i & !is.na(EngagementScore))) %>% 
  mutate(NetGainOrLoss = Count2015 - Count2013) %>% 
  mutate(PercentageChange = ((Count2015-Count2013)/(Count2013))* 100))
  saveRDS(get(i), paste0("data/",i,".rds"))
}

# Combine all the organization state tables very inelegantly

CombinedStates <- bind_rows(AG=AG,AGRI=AGRI,ARR=ARR,AVED=AVED,BCPSA=BCPSA,
                            CFD=CFD,CSCD=CSCD,EAO=EAO,EBC=EBC,EDUC=EDUC,EM=EM,
                            EMBC=EMBC,ENV=ENV,FIN=FIN,FLNR=FLNR,GCPE=GCPE,HLTH=HLTH,
                            JTSTL=JTSTL,MIT=MIT,NGD=NGD,OMB=OMB,PGT=PGT,PO=PO,
                            PSSG=PSSG,SBRT=SBRT,SDSI=SDSI,TICS=TICS,TRAN=TRAN, .id="source")

CombinedStates <- rename(CombinedStates, ORGID15 = source)
CombinedStates1 <- inner_join(CombinedStates, OrgNameID %>% select(ORGANIZATION15, ORGID15), by = "ORGID15")
CombinedStates1 <- CombinedStates1 %>% mutate(PercentageChange = round(PercentageChange,digits=2))


# Save relevant dataframes as external rds files

saveRDS(MainTable3, "data/MainTable3.rds")
saveRDS(CountOutputTable1, "data/CountOutputTable1.rds")
saveRDS(long_MT3, "data/long_MT3.rds")
saveRDS(CombinedStates1,"data/CombinedStates1.rds")

CombinedStatesX2 <- semi_join(CombinedStatesX,MainTable3)