### 
# Preliminary stuff
###
library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(gmRi)
library(stringi)

# A land shapefile..
land_sf <- ne_states(c("united states of america", "canada"), returnclass = "sf") %>%
  st_transform(., crs = 32619)

###
# SpeciesShortName_VASTModelDescription_CMIP6_SSPX_YY_ClimateEnsembleStat.rds
# SpeciesShortName = Just what it seems, a shorthand for the species (e.g., Haddock, Cod, Lobster)
# VASTModelDescription = A description of the VAST model, mostly for Andrew. All of these should be "full".
# CMIP6 = Place holder in the event we run CMIP5. All of these will be CMIP6
# SSPX = First part of the SSP description, where "X" will change depending on the scenario used
# YY = Second part of the SSP description, where YY will change depending on the scenario used (e.g., SSP5_85)
# mean = A description of the ensemble member statistic used, which can be mean, 5th or 95th. For now, all of these will be "mean"

###
# Example workflow -- mapping density
###

# Annoying box nonsense. We aren't going to be able to use `here` because the files would overwhelm GitHub so we don't want them in the repo.
project_box_path<- "/Users/clovas/Library/CloudStorage/Box-Box/Mills Lab/Projects/COCA19_Projections/"
projection_res_path <- paste0(project_box_path, "projections/")


# Map RDS reading function to nest files into data frame
read_rds_func <- function(file_name) {
  out <- readRDS(paste0(file_name))
  return(out)
}

all_proj_res <- tibble("File_Path" = list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)) %>%
  mutate(., "Data" = map(File_Path, read_rds_func))

# Map naming function to extract species and scenario from file path
species_scen_func<-function(file_path){
  temp<-stri_sub(file_path, 95,999)
  out<-unlist(str_split(temp, "_mean.rds"))[[1]][1]
  return(out)
}

all_proj_res<-all_proj_res%>%
  mutate(., "Species_Scenario" = map(File_Path, species_scen_func))

# For now, we are mostly going to be using the results in the "Dens" object to make our maps and then also for cropping with community footprints to get our summaries of change. Let's pull out the density results as a new column
get_dens_func<- function(data){
  out <- data$Dens
  return(out)
}

all_proj_res <- all_proj_res %>%
  mutate(., "Density" = map(Data, get_dens_func))

names(all_proj_res$Density[[1]])

#Create gridded data (?) -> Convert points in sf polygon for plotting
bSquare <- function(x, a, coords = c("x", "y")) {
  a <- sqrt(a) / 2
  x_temp <- sf::st_as_sf(x, coords = coords, crs = 4326, remove = FALSE)
  x <- st_transform(x_temp, crs = 32619) %>%
    drop_na(Value)
  x <- x %>%
    mutate(., geometry = sf::st_buffer(geometry,
                                       dist = a,
                                       nQuadSegs = 1,
                                       endCapStyle = "SQUARE"
    ))
  return(x)
}

dens_grid_func<- function(dens_data, species_scen, baseline_years = seq(from = 2010, to = 2019), projection_years = c(2055, 2075), ...){
  
  if(FALSE){ 
    species_scen<- all_proj_res$Species_Scenario[[1]]
    dens_data <- all_proj_res$Density[[1]]
    baseline_years <- seq(from = 2010, to = 2019)
    projection_years<- c(2055, 2075)
    name_vector<-list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)
  } 
  
  # Going to want an "average" across the baseline years..
  base_data <- dens_data %>%
    #mutate(species_scen = species_scen)%>%
    filter(., format(Time, "%Y") >= min(baseline_years) & format(Time, "%Y") <= max(baseline_years)) %>%
    group_by(., Lat, Lon) %>%
    summarize(., "Baseline_Mean_Dens" = mean(Prob_0.5)) %>%
    pivot_longer(., -c(Lat, Lon), names_to = "Variable", values_to = "Value")
  
  # Now the future scenarios...
  for(i in seq_along(projection_years)){
    fut_data_temp <- dens_data %>%
      filter(., format(Time, "%Y") == projection_years[i]) %>%
      group_by(., Lat, Lon) %>%
      summarize(., "Temp_Name" = mean(Prob_0.5))
    colnames(fut_data_temp)[3] <- paste0("Projected_", projection_years[i], "_Mean_Dens")
    
    # "Wide" to "long"
    fut_data_temp <- fut_data_temp %>%
      pivot_longer(., -c(Lat, Lon), names_to = "Variable", values_to = "Value")
    
    if (i == 1) {
      fut_data_out <- fut_data_temp
    } else {
      fut_data_out <- bind_rows(fut_data_out, fut_data_temp)
    }   
  }
  
  # Bring em all together
  all_dens_data <- bind_rows(base_data, fut_data_out)
  
  # Create "grid cells"
  all_dens_grid <- bSquare(all_dens_data, a = 25000 * 25000, coords = c("Lon", "Lat"))
  
  # Add species scenario
  all_dens_grid <- all_dens_grid %>%
    mutate(species_scen = species_scen)
  
  return(all_dens_grid)
}

# Let's map that to each of the species...
all_proj_res <- all_proj_res %>%
  mutate(., "Dens_Grid" = map2(Density, Species_Scenario, dens_grid_func))

# Pull gridded density data from all_proj_res, unnest and create new data frame
all_dens_grid<- all_proj_res$Dens_Grid

# Transform from large list to tibble
all_dens_grid <- enframe(all_dens_grid) %>%
  select(!name)%>%
  unnest(value) %>%
  group_by(species_scen)%>%
  nest()

###
# Footprints
###
library(raster)
foot_stack <- raster::stack(paste0(project_box_path, "data/All VTR safe fishing footprints by community and gear type 2011-2015.grd"))

# Get the stack index for names with "All"
all_ind <- which(grepl("All", names(foot_stack)))
names(foot_stack)[all_ind]

# Let's reduce the raster stack to just these "All" gear footprints for each of the communities
all_foot_stack<- foot_stack[[all_ind]]

# Reclassify to binary "fished" and "not-fished" cells.
m <- c(0, Inf, 1,  -Inf, 0, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
all_foot_stack_bin<- raster::reclassify(all_foot_stack, rclmat) 

##CSL: Convert foot print to polygons and fortify to use in ggplot
# Create nested data frame?
all_footprints <- unstack(all_foot_stack_bin)
all_footprints<-tibble("footprints" = all_footprints)%>%
  mutate(names = names(foot_stack)[all_ind])

polygon_fun<-function(footprints){
  out<-rasterToPolygons(footprints, fun=function(x){x==1})
  return(out)
}

fortify_fun<-function(foot_polygons){
  out<-fortify(foot_polygons)
  return(out)
}

all_footprints<-all_footprints%>%
  mutate(foot_polygons = map(footprints, possibly(polygon_fun, NA)),
         fortified = map(foot_polygons, possibly(fortify_fun, NA)))%>%
  arrange(names)

# Dropping nulls
all_footprints<-all_footprints%>%
  drop_na()

footprint_plots_df<-all_footprints%>%
  dplyr::select(names, fortified)
nrow(footprint_plots_df)
footprint_plots<-vector("list", length = 98)
names(footprint_plots)<-paste(unique(all_footprints$names))

for(i in 1:98){
  print(i)
  
  data<-footprint_plots_df[i,]%>%
    unnest(fortified)
  
  footprint_plots[[i]]<-ggplot()+
    geom_path(data = data, aes(x=long, y=lat, group=group))+
    ggtitle(names(footprint_plots)[i])
}

footprint_plots[[23]] 

footprint_plots_df%>%
  select(names)%>%
  write.csv(., "footprint_names.csv")

# Read in revised csv
footprint_names<-read.csv("Data/footprint_names.csv")
all_footprints<-all_footprints%>%
  filter(names %in% footprint_names$names)

# Bring in landings data (species per port)
comm_res<-top_species%>%
  filter(!PORT == "LUBEC, ME")%>%
  group_by(PORT)%>%
  nest()%>%
  arrange(PORT)
comm_res<-comm_res%>%
  cbind(all_footprints%>%
          arrange(names))

# Transform CRS of footprints to match density
comm_footprints<-comm_res%>%
  select(PORT, fortified)%>%
  summarise(footprint = map(fortified, function(x){
    x<-st_as_sf(x, coords = c("long", "lat"), crs = 4326, remove=FALSE)
    x<-st_transform(x, crs = 32619)
    return(x)
  }))
comm_footprints<-comm_footprints%>%
  unnest(footprint)%>%
  select(PORT, geometry)%>%
  rename("foot_geom" = "geometry")%>%
  group_by(PORT)%>%
  nest(foot_geom = foot_geom)

# Combine density data and footprints
all_dens_grid <- all_dens_grid%>%
  mutate(., comm_footprints %>%
          nest(comm_footprints = PORT:foot_geom))

saveRDS(all_dens_grid, file="all_dens_footprinds.RDS")

# Now a plotting function

    


