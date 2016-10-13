# recoding sample org names to bird names

# load packages

library(dplyr)
library(foreign)

# read the data

setwd("C:/@data/GitHub/WES-engagement-states/sampledata")

#sampledata <- (read.csv("WES1315_test.csv", header=TRUE, na.strings='#NULL!'))

# read SPSS file directly, using the `foreign` package
# see http://www.milanor.net/blog/how-to-open-an-spss-file-into-r/
sampledata <- read.spss("WES1315_test.sav", 
                        to.data.frame = TRUE,
                        trim.factor.names = TRUE)
ls.str(sampledata)

# tidy up and omit a few variables

sampledata$ORGID13 <- as.character(sampledata$ORGID13)
sampledata$ORGID15 <- as.character(sampledata$ORGID15)

user.ids <- select_vars(names(sampledata), starts_with("USER"))
sampledata <- sampledata %>%
  select(-one_of(user.ids))

ls.str(sampledata)


# ASSIGNMENT OF ORG VALUE 
# - to mask reality

# step 0.1 - create new variable to identify those individuals who did not move organizations

sampledata <- sampledata %>%
  mutate(Org.change =
           case_when(.$ORGID13 == .$ORGID15
                     ~ "same org"))

ls(sampledata)

# check to see how many are recoded
sampledata %>%
  group_by(Org.change) %>%
  summarise(group.total = n())


write.csv(sampledata, file="sampledata.csv")

# step 0.5 - out of scope variable in 2013 or 2015 remains the same 
#          - so need to filter out <BCPUBLICSERVICE13> == 1> before steps 1-3

# assign ORG13 to randomly selected new ORG13

ORGID.values <- c("HERON", "JAY", "LOON", "RAVEN")

sampledata1 <-
  sampledata %>%
  filter(BCPUBLICSERVICE13 == "In") %>%
  mutate(ORGID13_new = sample(ORGID.values, nrow(), FALSE, prob=c(.25, .25, .25, .25))) %>%
  ungroup()
  

sampledata$ORGID13_new[sample(ORGID.values, nrow(sampledata), 
                              FALSE, prob=c(.25, .25, .25, .25))]


  

# -=-=-=-=-=-

# you are now entering the "experimental / not connected to the earlier code" section


# step 99 - recode some (25%) of the low engagement in 2013 folks to "HERON"
#        - objective is to have some of them to improve

sampledata1 <-
  sampledata %>%
  filter(BCPUBLICSERVICE13 == "In")

head(sampledata1)

sampledata2 <- 
  sampledata1 %>%
  filter(Engagement_13 < 50)

head(sampledata2)

sampledata3 <-
  sampledata2 %>%
  sample_frac(.1)

head(sampledata3)

sampledata3$ORGID13 <- "HERON"
sampledata3$ORGANIZATION13 <- "Ministry of Heron"

head(sampledata3)

# string it all together

sampledata4 <-
  sampledata %>%
  filter(BCPUBLICSERVICE13 == "1") %>%
  filter(Engagement_13 < 50) %>%
  sample_frac(.1) %>%
  mutate(ORGID13 = "HERON") 

head(sampledata4)

# but that's only the 45 (10% sub-sample) 

sampledata5 <-
  sampledata %>%
  mutate(ORGID13 =
         case_when(.$BCPUBLICSERVICE13 == "1" & .$Engagement_13 < 50 
                   ~ "HERON"))

head(sampledata5)
tail(sampledata5)

sampledata5 %>%
  group_by(ORGID13) %>%
  summarise(total.count=n())

# it works!

# now with a sample_frac(.1) added to the case_when

sampledata5 <-
  sampledata %>%
  mutate(ORGID13 =
           case_when(.$BCPUBLICSERVICE13 == "1" & .$Engagement_13 < 50 
                     ~ "HERON"))

head(sampledata5)
tail(sampledata5)

sampledata5 %>%
  group_by(ORGID13) %>%
  summarise(total.count=n())


# add conditional as last step?

# step 2 - random assignment of remainder of 2013 folks to ORG
#          -- HERON
#          -- LOON
#          -- OWL
#          -- PUFFIN
#          -- RAVEN

# step 3 - assignment of remainder to 2015
#        - probability of 70% that they stay in same ORG
#        - else repeat step 2


