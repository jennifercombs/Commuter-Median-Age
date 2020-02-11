library(tidyverse)
library(tidycensus)

variables <- load_variables(dataset = "acs5", year = 2018)

commute2018 <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c(total = "B08103_001",
                car_alone = "B08103_002",
                carpool = "B08103_003",
                publictransit = "B08103_004",
                walked = "B08103_005",
                population = "B01003_001"),
  year = 2018) %>% 
  select(-moe, -GEOID) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(median = total) %>% 
  arrange(-population) %>% 
  slice(1:50) %>% 
  gather(
    key = "key",
    value = "value",
    -NAME, -population, -median)

commute2018$NAME <- str_remove(commute2018$NAME, " Metro Area")
commute2018$key <- factor(commute2018$key, levels = c("walked", "publictransit", "carpool", "car_alone", "total"))

ggplot(commute2018) +
  geom_point(
    aes(
      x = reorder(NAME, -median),
      y = value,
      color = key),
    size = 2)+
  coord_flip()+
  scale_color_manual(values = c("#aa264b", "#e79b42", "#99a783","#3d6a6b", "grey90"),
                     labels = c("Walk", "Public transit", "Carpool", "Drive alone", "All commuters"))+
  theme_minimal()+
  labs(title = "Variation in median age of commuters by mode",
       subtitle = "Comparison of the 50 largest Metropolitan Statistical Areas in the US",
       y = "Median age",
       caption = "Source Data: 2018 ACS 5-year estimates, US Census Bureau")+
  theme(text = element_text(family = "SourceSansPro-Light", 
                            color = "grey10", 
                            lineheight = 0.5),
        plot.title = element_text(family = "SourceSansPro-Regular", 
                                  size = 15, 
                                  hjust = -2),
        plot.subtitle = element_text(size = 10, 
                                     hjust = -1.61),
        plot.caption = element_text(size = 6, 
                                    color = "grey50"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line(color= "grey95", 
                                          size = .2),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank())


ggplot2::ggsave("output.jpg", height = 8, width = 8, units = "in")

