




# lt function2 (tim) 

#remotes::install_github("timriffe/DemoTools")
source("codes/0. Settings.R")
source("codes/LifeTableFUN.R")
load("data/output/data.RData")
head(data)




# trial to estimate 1 life expectancy
tt <- data %>% filter(time==2019, sex=="F", geo=="ES23") %>% arrange(age2)
xx <- lifetable2(x=tt$age2, Nx=tt$values, Dx=tt$Deaths2, sex=="f") %>% mutate(SGT="999")
head(xx)



lifetable2 <- function(x, Nx, Dx, sex = "m"){
  require(DemoTools)
  # Use an HMD-based rule-of thumb to split age 0-4 into [0, 1-4]
  m0_4 <- Dx[1] / Nx[1]
  m0_1 <- lt_rule_4m0_m0(M04 = m0_4, D04 = Dx[1], P04 = Nx[1], Sex = sex)
  D0_1 <- lt_rule_4m0_D0(M04 = m0_4, D04 = Dx[1], P04 = Nx[1], Sex = sex)
  E0_1 <- D0_1 / m0_1
  D_abr <- c(D0_1, Dx[1] - D0_1, Dx[-1])
  E_abr <- c(E0_1, Nx[1] - E0_1, Nx[-1])
  age_abr <- c(0,1,x[-1])
  
  lt_abridged(Deaths = D_abr,
              Exposures = E_abr,
              Age = age_abr,
              axmethod = "un",
              Sex = sex)
}


xx2 <- lifetable2(x=tt$Edad, Nx=tt$Total, Dx=tt$Defunciones, sex="m") 
head(xx2)






head(dat)


dat2 <- dat %>% mutate(yp=paste(Periodo, Provincias), 
                       Defunciones=ifelse(Defunciones==0, 0.0000001, Defunciones))
head(dat2)

dat2 %>% arrange(Defunciones) %>% head(n=50)


for (i in names(table(dat2$yp))){
  xxx <- lifetable2(x=dat2[dat2$yp==i & dat2$Sexo=="Hombres",]$Edad, 
                    Nx=dat2[dat2$yp==i & dat2$Sexo=="Hombres",]$Total, 
                    Dx=dat2[dat2$yp==i & dat2$Sexo=="Hombres",]$Defunciones, sex="m") %>% as.data.frame() %>%
    mutate(yp=i, Sexo="Hombres")
  
  xxxx <- lifetable2(x=dat2[dat2$yp==i & dat2$Sexo=="Mujeres",]$Edad, 
                     Nx=dat2[dat2$yp==i & dat2$Sexo=="Mujeres",]$Total, 
                     Dx=dat2[dat2$yp==i & dat2$Sexo=="Mujeres",]$Defunciones, sex="m") %>% as.data.frame() %>%
    mutate(yp=i, Sexo="Mujeres")
  xx2 <- bind_rows(xx2, xxx, xxxx); #xx <- xx %>% filter(sd!=9999)
  
}

head(xx2)



