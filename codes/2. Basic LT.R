
source("codes/0. Settings.R")
source("codes/LifeTableFUN.R")
load("data/output/data.RData")
head(data)




# trial to estimate 1 life expectancy
tt <- data %>% filter(time==2019, sex=="F", geo=="ES23") %>% arrange(age2)
xx <- lifetable(x=tt$age2, Nx=tt$values, Dx=tt$Deaths2) %>% mutate(SGT="999")
head(xx)

# estimate life expectancies
for (i in names(table(data$SGT))){
  xxx <- lifetable(x=data[data$SGT==i,]$age2, 
                   Nx=data[data$SGT==i,]$values, 
                   Dx=data[data$SGT==i,]$Deaths2) %>%
    mutate(SGT=i)
  xx <- bind_rows(xx, xxx); 
  
}

xx %>% head(n=60) # 15.04

save(xx, file="data/output/basic LT.RData")



# 12.59
# max 2.5h


