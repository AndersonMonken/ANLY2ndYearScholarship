rm(list=ls())
library(feather)
library(tigris)
library(tidyverse)
library(sp)
library(rgeos)
setwd('/home/anderson/ANLY2ndYearScholarship/')


df_fips_codes <- read_feather('fips_codes.feather')
df_fips <- read_feather('df_county_data.feather') %>% mutate(GEOID = fips)
df_pp <- read_feather('pp_data.feather') %>% left_join(df_fips_codes[c('fips','county','state')])
df_superfund <- read_feather('superfund_data.feather') %>% left_join(df_fips_codes[c('fips','county','state')])

saveRDS(df_pp,'pp_data.rds')
saveRDS(df_superfund,'superfund_data.rds')


us.map <- tigris::counties(cb = TRUE, year = 2015)

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

us.map <- us.map[!us.map$STATEFP %in% c("72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),] #"02", "15", 


fips_latlong <-gCentroid(us.map,byid=TRUE) %>% 
  coordinates() %>% as.data.frame() %>% 
  dplyr::rename('longitude' = x, 'latitude' = y)
us.map$latitude <- fips_latlong$latitude
us.map$longitude <- fips_latlong$longitude

map_data <- merge(us.map, df_fips, by=c("GEOID"))

test <- as.data.frame(map_data) %>% 
  mutate(region = case_when(state %in% c('CT','ME','MA','NH','RI','VT', 'NJ','NY','PA') ~ 'Northeast',
                            state %in% c('IL','IN','MI','OH','WI',  'IA','KS','MN','MO','NE','ND','SD') ~ 'Midwest',
                            state %in% c('DE','FL','GA','MD','NC','SC','VA','DC','WV',  'AL','KY','MS','TN', 'AR','LA','OK','TX') ~ 'South',
                            state %in% c('AZ','CO','ID','MT','NV','NM','UT','WY', 'AK','CA','HI','OR','WA') ~ 'West'
),
division = case_when(state %in% c('CT','ME','MA','NH','RI','VT') ~ 'New England',
                     state %in% c('NJ','NY','PA') ~ 'Mid-Atlantic',
                     state %in% c('IL','IN','MI','OH','WI') ~ 'East North Central',
                     state %in% c('IA','KS','MN','MO','NE','ND','SD') ~ 'West North Central',
                     state %in% c('DE','FL','GA','MD','NC','SC','VA','DC','WV') ~ 'South Atlantic',
                     state %in% c('AL','KY','MS','TN') ~ 'East South Central',
                     state %in% c('AR','LA','OK','TX') ~ 'West South Central',
                     state %in% c('AZ','CO','ID','MT','NV','NM','UT','WY') ~ 'Mountain',
                     state %in% c('AK','CA','HI','OR','WA') ~ 'Pacific'
),
epa_region = case_when(state %in% c('CT','ME','MA','NH','RI','VT') ~ 1,
                     state %in% c('NJ','NY') ~ 2,
                     state %in% c('PA','DE','MD','VA','DC','WV') ~ 3,
                     state %in% c('FL','GA','NC','SC','AL','KY','MS','TN') ~ 4,
                     state %in% c('IL','IN','MI','MN','OH','WI') ~ 5,
                     state %in% c('AR','LA','OK','TX','NM') ~ 6,
                     state %in% c('IA','KS','MO','NE') ~ 7,
                     state %in% c('CO','MT','UT','WY','ND','SD') ~ 8,
                     state %in% c('AZ','CA','NV','HI') ~ 9,
                     state %in% c('AK','OR','WA','ID') ~ 10
), epa_region = as.factor(epa_region)
)

map_data$division <- test$division
map_data$region <- test$region
map_data$epa_region <- test$epa_region

saveRDS(map_data,'map_data.rds')

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    map_data$county, 
                    "<br><strong>Cancer Rate (Age Adjusted) Out of 100,000: </strong>", 
                    map_data$cancer_per100k,
                    "<br><strong># Superfund Sites: </strong>", 
                    map_data$num_sites,
                    "<br><strong># Powerplants: </strong>", 
                    map_data$num_pp)
saveRDS(popup_dat,'popup_cancer.rds')
# set up label for national park points
# popup_SF <- paste0("<strong>Superfund Name: </strong>", 
#                    df_fips$park, 
#                    "<br><strong>Type: </strong>", 
#                    natparks.points$type)

bins_sites <- c(0, 1, 2, 3, 5, 8, 10, 15, 20, Inf)
pal_sites <- colorBin("YlOrRd", domain = map_data$num_sites, bins = bins_sites)
bins_pp <- c(0, 1, 5, 12, 25, 36, 51, 75, Inf)
pal_pp <- colorBin("YlOrRd", domain = map_data$num_pp)
#pal_pp <- colorQuantile("YlOrRd", NULL, n = 4)

pal_cancer <- colorQuantile("YlOrRd", NULL, n = 9)

k <- 3

map <- leaflet(data = map_data) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addPolygons(fillColor = ~pal_sites(num_sites), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="Number of Superfund Sites") %>% 
  addPolygons(fillColor = ~pal_cancer(cancer_per100k), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="Cancer Rate/100,000 by Counties") %>% 
  addPolygons(fillColor = ~pal_pp(num_pp), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="Number of Powerplants") %>%
  addMarkers(data=map_data,lat=~latitude, lng=~longitude, popup=popup_dat, group = "FIPS TEST")


map %>% 
  addLayersControl(
    position = c('topright'),
    baseGroups = c("Cancer Rate/100,000 by Counties","Number of Superfund Sites","Number of Powerplants"), #overlayGroups = c("Cancer Rate/100,000 by Counties","Number of Superfund Sites"),
    options = layersControlOptions(collapsed = FALSE)) # %>% 
  #hideGroup(c("Number of Superfund Sites")) #hide all groups except the 1st one

# %>% addMarkers(data=df_fips,lat=~latitude, lng=~longitude, popup=popup_SF, group = "Superfund Sites")
