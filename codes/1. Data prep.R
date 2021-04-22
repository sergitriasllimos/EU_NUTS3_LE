# get and prepare data

source("codes/0. Settings.R")


# population
pop <- get_eurostat("demo_r_pjangrp3", time_format = "num")
head(pop)

# deaths (data prepared by Tim)
# this data set is currently in my drive
deaths  <- readRDS("D:/CED/With other people/COVID-19/NUTS-3 EU/eurostat_mort.rds")
head(deaths)

total <- left_join(pop, deaths) %>%
  mutate(age2=recode(age, "Y_LT5"="0", "Y5-9"="5", "Y10-14"="10", 
                     "Y15-19"="15", "Y20-24"="20", 
                     "Y25-29"="25", "Y30-34"="30", 
                     "Y35-39"="35", "Y40-44"="40", 
                     "Y45-49"="45", "Y50-54"="50", 
                     "Y55-59"="55", "Y60-64"="60", 
                     "Y65-69"="65", "Y70-74"="70", 
                     "Y75-79"="75", "Y80-84"="80",
                     "Y85-89"="85", "Y_GE90"="90" )) %>%
  filter(age!="TOTAL", age!="UNK", age!="Y_GE85") %>%
  mutate(age2=as.numeric(age2), 
         SGT=paste(sex, time, geo)) %>%
  arrange(age2)
head(total)
table(total$age2)


# Get deaths with unknown age and redistribute them
total.unk <- left_join(pop, deaths) %>%
  mutate(age2=recode(age, "Y_LT5"="0", "Y5-9"="5", "Y10-14"="10", 
                     "Y15-19"="15", "Y20-24"="20", 
                     "Y25-29"="25", "Y30-34"="30", 
                     "Y35-39"="35", "Y40-44"="40", 
                     "Y45-49"="45", "Y50-54"="50", 
                     "Y55-59"="55", "Y60-64"="60", 
                     "Y65-69"="65", "Y70-74"="70", 
                     "Y75-79"="75", "Y80-84"="80",
                     "Y85-89"="85", "Y_GE90"="90" )) %>%
  filter(age=="UNK") %>%
  select(sex, time, geo, deaths.unk=Deaths)
head(total.unk)
table(total$age2)

data <- total %>% left_join(total.unk) %>% 
  group_by(sex, time, geo) %>%
  mutate(w=Deaths/sum(Deaths)) %>% ungroup() %>% 
  mutate(Deaths2=Deaths + (deaths.unk * w))%>%
  mutate(age2=as.numeric(age2), 
         SGT=paste(sex, time, geo)) %>%
  arrange(age2)
head(data)


save(data, file="data/output/data.RData")


