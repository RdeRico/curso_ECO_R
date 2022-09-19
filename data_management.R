library('tidyverse')
library('here')
library('readxl')
library('tidylog')
library('summarytools')
library('knitr')

waste<- read_csv(here('datasets/country_level_data.csv'))
glimpse(waste)

head(waste)
tail(waste)



#waste %>% rename(population = population_population_number_of_people)

waste_select <- waste %>%
  select(iso3c,
         region_id,
         country = country_name,
         income_id,
         gdp,
         population = population_population_number_of_people,
         total_waste = total_msw_total_msw_generated_tons_year,
         starts_with('composition'))%>%
  arrange(desc(total_waste))
  
waste_regions <- waste_select %>%
  mutate(region_id = recode(region_id,
                            'LCN' = 'Latin_America',
                            'SAS' = 'South_asia',
                            'SSF' = 'Sub-Saharan_Africa',
                            'ECS' = 'Europe_Central_Asia',
                            'MEA' = 'Middle_East_North_Africa',
                            'EAS' = 'East_Asia_Pacific',
                            'NAC' = 'North_America'))

waste_regions %>% 
  group_by(region_id)%>%
  summarise(total_waste=sum(total_waste, na.rm = T))%>%
  mutate(waste_tmtons= total_waste/1000000)


waste_select %>%
  filter(income_id %in% c('HIC','LIC','LMC') )

waste_regions%>%
  mutate(pop_size=case_when(
    population>= 1000000~'big',
    population<  1000000 & population >500000~'medium',
    population<= 500000~'small'))%>%
  relocate(pop_size, .before = population)


world_data <- read_csv2(here('datasets/world_data.csv'))
glimpse(world_data)

continent <- world_data %>%
  select(iso_a3, country_name = name_long,continent)
glimpse(continent)

waste_world <- waste_regions %>%
  rename(iso_a3= iso3c)%>%
  left_join(continent, by = 'iso_a3')
waste_world %>% 
  filter(is.na(continent))

continent_corrected <- continent %>%
  mutate(iso_a3 =ifelse( country_name== 'Kosovo', 'XKX', iso_a3),
         iso_a3 =ifelse( country_name== 'Taiwan', 'TWN', iso_a3))
library('stringr')
continent%>%
  filter(str_detect(country_name,'Channel Island|Kosovo|Gibraltar|Tuvalu|Taiwan'))

write.csv(waste_world, here('datasets/waste_world.csv'))


composition <- waste_regions%>%
  pivot_longer(cols = starts_with('composition'), names_to='composition',
               values_to = 'percent')
glimpse(composition)

composition%>%
  mutate(composition = str_remove(composition, 'composition_'))%>%
  mutate(composition = str_remove(composition, '_percent'))


composition %>%
  group_by(country) %>%
  mutate(per_sum=sum(percent,na.rm = T))%>%
  filter(per_sum > 99.9)%>%
  filter(per_sum < 100.1)

  
