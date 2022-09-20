library(readr)
library(tidyverse)
library(ggplot2)
library(here)
library(RColorBrewer)
library(rcartocolor)
library(MetBrewer)


but<-read_csv(here('datasets/butterfly_dataset.csv'))

but <- but %>%
  mutate(date = str_c(year, '-', month, '-', day))%>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'))

but_sum <- but %>%
  group_by(plot, habitat, hab_type, date) %>%
  summarise(n_species = n_distinct(species), 
            abundance = sum(abundance))

#ejercicio 1
ggplot(data = but_sum, aes(x = date,
                           y = n_species,
                           size =abundance))+
  geom_point()
#ejercicio 2
ggplot(but_sum, aes(n_species, abundance, color = hab_type))+
  geom_point()+
  scale_color_carto_d()+
  scale_y_log10()+
  scale_x_log10()
#ejercicio 3
library(plotly)
ggplotly()

ggplot(but_sum, aes(date, n_species, color = abundance))+
  geom_point()+
  scale_color_distiller(palette = 'RdYlBu')+
  labs(x = 'Año',
       y = 'Número de especies',
       title = 'Muestreos de mariposas en Vietnam',
       subtitle = 'Datos de Bonebrake et al. 2016',
       color = 'Abundancia')
