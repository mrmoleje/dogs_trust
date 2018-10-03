#Sue Wallace
#29.09.2018

# Dog's Trust Hackathon

# Load libraries----

install.packages("ggplot2")

library(readxl)
library(dplyr)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

# Read in dogs trust data and take out the row with no location data

rehoming <- read_excel("Data/Dogs Trust Rehoming Data.xlsx") 


# Postcode lookups

centre_loc <- read_excel("Data/centre_latlon.xlsx")


# De-duplicate the data

dog_dedupe <- rehoming %>%
  group_by(Animal_Code, Visit_Number) %>%
  filter(row_number()==n()) %>%
  ungroup()

# Join the postcode lat long data 

rehoming_plus <- dplyr::left_join(
  x = dog_dedupe,  # to this table...
  y = centre_loc,   # ...join this table
  by = "Rehoming_Centre"  # on this key
) 

# Are particular breeds more likely to be rehomed in particular areas of the UK?

# Could also look at colours of dogs, size, gender

# Is there a correlation between the dog breed and the number of days to 
# rehome?

# number of days to rehome

rehoming_plus$diff_in_days <- difftime(rehoming_plus$Visit_Date_Out,
                                       rehoming_plus$Visit_Date_In, units = c("days"))

# Groups for analysis

rehoming_plus %>%
  group_by(Rehoming_Centre, Kennel_Club_Group) %>% 
  summarise(count=n()) -> breed_grouping

rehoming_plus %>%
  na.omit() %>% 
  group_by(Rehoming_Centre, lat, lon) %>% 
  summarise(count=n()) -> centres

# what about average number of days to rehome per centre. Then a filter
# for each breed using crosstalk

rehoming_plus %>%
  na.omit(Kennel_Club_Group, Rehoming_Centre) %>% 
  group_by(Rehoming_Centre, Kennel_Club_Group) %>% 
  summarise(round(mean(diff_in_days))) %>% 
  rename(Mean= "round(mean(diff_in_days))", 
         Breed = Kennel_Club_Group, Centre = Rehoming_Centre)-> days_to_adopt

# Plot the centres on a map

# Call RColorBrewer::display.brewer.all() to see all possible palettes

# colour palette will need to match number of centres

colourCount = length(unique(centres$Rehoming_Centre))
getPalette = colorRampPalette(brewer.pal(9, "Spectral"))

pal <- colorFactor(
  palette = getPalette(colourCount),
  domain = centres$Rehoming_Centre
)

# Create a map

leaflet(data = centres) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, weight = 3, 
                   color = ~pal(Rehoming_Centre),
                   stroke = TRUE, fillOpacity = 0.5, 
                   radius = ~ifelse(count >= 1, 6, 10), # could say here if count >1000 then radius x, if count <1000
                   popup = ~paste0("<h5>", Rehoming_Centre, "</h5>")) %>%
  addLegend("bottomleft", pal = pal, values = ~Rehoming_Centre,
            title = "Rehoming Centre",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 2
  ) 


# could I make a filterable chart, with the centres on the x, and the 
# average number of days to adopt per breed on the y - which is filterable by
# breed. 

days_to_adopt$Breed[is.na(days_to_adopt$Breed)] <- "Unknown"


days_to_adopt %>%
  ggplot(aes(x = Centre, y = Mean, fill = Breed)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Spectral") +
  theme_classic()+
  theme(text = element_text(size=12)) -> p

p <- ggplotly(p)
