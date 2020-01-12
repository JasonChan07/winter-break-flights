library(dplyr)
library(ggplot2)
library(data.table)
library(knitr)
library(geosphere)
library(rgdal)
library(raster)
library(sf)
library(ggalt)


setwd('/Users/jasonchan/Documents/DataProjects/flights')

# load data 
airports_original <- data.table::fread('airports.csv', data.table = FALSE)
routes_original <- data.table::fread('routes.csv', data.table = FALSE)

#########################
### CLEAN ROUTES DATA ###g
#########################

routes <- routes_original %>% 
            # keep source and destination columns
            dplyr::select(V4, V6) %>% 
            # change column type to numeric
            mutate_all(as.numeric) %>% 
            # rename columns
            rename('source_id' = V4, 'dest_id' = V6)


###########################
### CLEAN AIRPORTS DATA ###
###########################

airports <- airports_original %>% 
                # keep airport names, airport id's, coordinates
                dplyr::select(V1, V2, V3, V5, V7, V8) %>%
                # rename columns
                rename('id' = V1, 'name'= V2, 'city' = V3, 'iata' = V5, 'lat' = V7, 'long' = V8)


###############
### COMBINE ###
###############

flights <- routes %>% 
                # obtain city, iata, lat, long for source airports
                inner_join(dplyr::select(airports, id, city, iata, lat, long),
                             by = c('source_id' = 'id')) %>% 
                # rename columns
                rename('source_city' = city, 'source_iata' = iata, 'source_lat' = lat, 'source_long' = long) %>% 
                # obtain city, iata, lat, long for dest airports
                inner_join(dplyr::select(airports, id, city, iata, lat, long),
                            by = c('dest_id' = 'id')) %>% 
                # rename column
                rename('dest_city' = city, 'dest_iata' = iata, 'dest_lat' = lat, 'dest_long' = long) %>% 
                # add column for routes
                mutate(route = paste(source_city, dest_city, sep = '-')) %>% 
                # only keep distinct routes
                distinct(route, .keep_all = TRUE)

## Given a city and destination, generate schema
generateFlights <- function(source, dest) {
    # obtain source city schema
    source_schema <- flights %>% 
                        filter(source_iata == source) %>% 
                        dplyr::select(source_city, source_iata, source_long, source_lat)
    dest_schema <- flights %>% 
                        filter(dest_iata == dest) %>% 
                        dplyr::select(dest_city, dest_iata, dest_long, dest_lat)
    final_schema <- cbind(source_schema[1, ], dest_schema[1, ]) %>% 
                        mutate(route = paste(source_city, dest_city, sep = '-')) %>% 
                        dplyr::select(route, everything(), -source_city, -dest_city)
    
    return(final_schema)
}

# manual flights
manual_flights <- generateFlights(source = 'DCA', dest = 'CMN') %>% 
                rbind(generateFlights(source = 'CMN', dest = 'OPO'),
                      generateFlights(source = 'LHR', dest = 'MAA')
                )


# flights taken since semester over
trip_flights <- flights %>% 
    filter(source_iata == 'SFO' & dest_iata == 'DCA' | # shohini to DC
               source_iata == 'SFO' & dest_iata == 'DEN' | # jason, kiran to london
               source_iata == 'DEN' & dest_iata == 'LHR' | 
               source_iata == 'LAX' & dest_iata == 'DCA' | # prabha to DC
               source_iata == 'IAD' & dest_iata == 'LHR' | # prabha to London
               source_iata == 'SFO' & dest_iata == 'LHR' | # jeffrey, mihir to london
               source_iata == 'LHR' & dest_iata == 'OPO' | # everyone to porto, technically stansted
               source_iata == 'LIS' & dest_iata == 'DXB'| # shohini to dubai
               source_iata == 'DXB' & dest_iata == 'DEL' |
               source_iata == 'LIS' & dest_iata == 'LHR' | # everyone to london, technically luton
               source_iata == 'LHR' & dest_iata == 'BOM' | # jeffrey, mihir to mumbai 
               source_iata == 'LHR' & dest_iata == 'MUC' | # jason to munich
               source_iata == 'MUC' & dest_iata == 'SFO') %>%  # jason to sfo
    dplyr::select(route, source_iata, source_long, source_lat, dest_iata, dest_long, dest_lat) %>% 
    rbind(manual_flights)

# cities for plotting
cities <- airports %>% 
            filter(iata %in% trip_flights$source_iata | iata %in% trip_flights$dest_iata)
            
                    
                    
#############
### PLOT ####
#############  

# get great circles for each flight
gc_routes <- geosphere::gcIntermediate(trip_flights[c("source_long", "source_lat")],
                                       trip_flights[c("dest_long", "dest_lat")],
                                       n = 360, addStartEnd = TRUE, sp = TRUE, 
                                       breakAtDateLine = TRUE)

gc_routes_spatial <- sp::SpatialLinesDataFrame(gc_routes, 
                                               data.frame(route = trip_flights$route,
                                                          stringsAsFactors = FALSE))

# load world shape file
world <- raster::shapefile('world/ne_110m_admin_0_countries_lakes.shp')

# convert to dataframe
world_df <- fortify(world)
gc_routes_df <- fortify(gc_routes_spatial)

groups <- c('Shohini', 'Mihir & Jeffrey', 'Kiran & Jason', 'Prabha', 'Prabha', 'Jason',
           'Kiran, Jason, Mihir, Jeffrey, Prabha', 'Mihir & Jeffrey', 'Kiran, Jason, Mihir, Jeffrey, Prabha',
           'Shohini', 'Kiran & Jason', 'Jason', 'Shohini', 'Shohini', 'Shohini', 'Kiran & Prabha')

group_ids <- data.frame(id = unique(gc_routes_df$id), groups)

# join gc_routes_df and name_ids
gc_routes_final <- gc_routes_df %>%
                    inner_join(group_ids, by = 'id') %>%
                    mutate(groups = as.factor(groups))


ggplot() +
    # world
    geom_polygon(data = world_df, aes(long, lat, group = group), 
                 fill = "grey80", color = "grey60", size = 0.1) +
    # city points
    geom_point(data = cities, aes(long, lat), color = "grey20", size = 0.5) +
    # city text 
    geom_text(data = cities, aes(long, lat, label = city),
              size = 3, color = "grey20", alpha = 0.9, nudge_y = 2, 
              check_overlap = TRUE) +
    # routes
    geom_path(data = gc_routes_final, 
              aes(long, lat, group = group, colour = groups), alpha = 0.5) +
    # aesthetics
    coord_proj("+proj=kav7") +
    scale_x_continuous(breaks = seq(-180, 180, 30)) +
    scale_y_continuous(breaks = seq(-90, 90, 15)) +
    theme(panel.grid.major = element_line(size = 0.5, linetype = 2),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
