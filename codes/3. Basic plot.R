
load("data/output/basic LT.RData")




xx %>% head()
as <- xx %>% mutate(sex=substr(SGT, 1,1),
                    year=substr(SGT, 3,6),
                    geo=substr(SGT, 8,12)) %>% filter(x==0); head(as)

as %>% filter(grepl("ES", geo), year==2019, sex=="F")
as %>% filter(geo=="ES23")
summary(as$ex)

# example Spain
as %>% filter(grepl("ES", geo), x==0, year %in% c(2019, 2020)) %>%
  select(sex, year, geo ,ex) %>% 
  spread(year, ex) %>% 
  mutate(dif=`2020`-`2019`) %>% arrange(dif) %>%
  filter(grepl("ES5", geo))


# results e0
results <- as %>% filter( x==0, year!=9) %>%
  select(sex, year, geo ,ex) %>% unique() %>%
  spread(year, ex) %>% 
  mutate(mm = (`2019` + `2018` + `2017`)/3, dif1719=`2020`-mm,
         dif=`2020`-`2019`) %>% arrange(dif) 
head(results)

results %>% filter(grepl("ES7", geo))
results %>% filter(grepl("FR", geo)) %>% arrange(-dif)

table(results$sex)
summary(results$dif)



geodata <- get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
                                   nuts_level = 3,
                                   year = 2021)

results <- results %>% mutate(NUTS_ID=geo)

map_data <- inner_join(geodata, results)
head(map_data)
map_data %>% filter(grepl("FR", NUTS_ID), sex!="T") %>% head()

# Life expectancy changes 2020 vs 2019
ggplot(data = map_data  %>% filter(sex!="T", dif<4, dif>-7), 
       aes(fill = as.factor(round(dif,0)))) + 
  geom_sf() + coord_sf(datum = NA) +
  theme_minimal() + facet_grid(.~sex) + theme(rect = element_blank()) +
  #scale_fill_brewer(palette="BrBG", direction=-1) +
  labs(fill="Life expectancy changes (in years)")


# Life expectancy changes 2020 vs 2017-19
ggplot(data = map_data  %>% filter(sex!="T", dif1719<4, dif1719>-7), 
       aes(fill = as.factor(round(dif1719,0)))) + 
  geom_sf() + coord_sf(datum = NA) +
  theme_minimal() + facet_grid(.~sex) + theme(rect = element_blank()) +
  #scale_fill_brewer(palette="BrBG", direction=-1) +
  labs(fill="Life expectancy changes (in years)")
