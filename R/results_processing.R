#####
## Results processing kickstarter
#####

### 
# Preliminary stuff
###
library(tidyverse)
library(viridis)
library(raster)
library(sf)
library(terra)
library(rnaturalearth)

# A land shapefile..
land_sf <- ne_states(c("united states of america", "canada"), returnclass = "sf") %>%
    st_transform(., crs = 32619)


###
# Overview
###
# We've got MODELS!! Now, what to do with em :) Just as a quick recap, for each species, we fit the vector auto-regressive spatio-temporal models to a dataset that includes the spring and fall NOAA NEFSC bottom trawl data and Fisheries and Oceans Canada spring and summer bottom trawl data. Within this model structure, we have included depth, seasonal average sea surface temperature and seasonal average bottom temperature as habitat covariates (i.e., things we think influence the true, unobserved biomass of a species at a given location/time). We have also included survey as a factor cathability covariate to account for systematic differences between the NOAA NEFSC bottom trawl and the DFO bottom trawl. Nex, we have estimated unmeasured spatial variability, and spatio-temporal variability. Finally, we have accounted for a random walk autoregressive process in the model intercepts (i.e., average occurrence of the species at a given time step across the entire survey domain). 

# After fitting the VAST model, we then make our projections using two different climate scenarios run as part of the CMIP6 effort: one scenario is the shared socio-economic pathway (SSP) 1_2.6 and the other is SSP5_8.5. Within each scenario, a number of different climate model members are available and we calculate the average, 5th, and 95th percentiles across the ensemble of individual runs. Projections are made for every season-year from 1985 to 2100 using the seasonal average bottom and sea surface temperatures from the CMIP6 efforts. There's some more detail on the climate data processing steps here (https://gulfofmaine.github.io/sdm_workflow/docs/environmental-data-collection.html), particularly how we used the delta method to try to correct for the well known-warm bias of climate models within the northwest Atlantic. 

# Once we have completed the projection process, we end up with a few RData files for each species that will all have the same naming convention:
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
# Feel free to work with these projection files in whichever way works best for you to get to the end goal! Below is just an example to show you what is in these rds files and how I have visualized some of the projection data before. 

# Annoying box nonsense. We aren't going to be able to use `here` because the files would overwhelm GitHub so we don't want them in the repo.
project_box_path<- "/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/COCA19_Projections/"
projection_res_path <- paste0(project_box_path, "projections/")

# In the end, I think the most efficient thing is going to be to have a nested dataframe and then write some functions that we can "map" over the rows of the dataframe. So, creating that...
read_rds_func <- function(file_name) {
    out <- readRDS(paste0(file_name))
    return(out)
}

all_proj_res <- tibble("File_Path" = list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)) %>%
mutate(., "Data" = map(File_Path, read_rds_func))

# Look at just one of these objects to see its structure
str(all_proj_res$Data[[1]]) # Unproductive...
names(all_proj_res$Data[[1]])

# Okay, a little bit more helpful. The .rds files are lists with the summarized projection results. For each species/climate scenario/stat, we stored a number of different results including:
# Index: The projected overall biomass index, with columns for time, Region, Prob_0.5 (mean across the 100 projection simulations to represent uncertainty in fitted VAST model), Prob_0.1 (10th percentile across 100 simulations), Prob_0.9 (90th percentile across 100 simulations). We will likely just use Prob_0.5.
# Dens: The projected density (kg per km2) at each grid cell, with columns for the Lat, Lon of the grid cell, time, and then similar probability measures as index.
# COG: Ignore for now
# COG_True: The center of gravity, with columns for time, and then we have the Lon/Lat median, 10th and 90th percentiles across the 100 projection simulations. As above, we are just going to use Lon_Prob_0.5 and Lat_Prob_0.5
# EffArea: Ignore for now
# EffArea_True: The effective area occupied, with columns for Time, Region, and then the probability measures. Again, we will just use "Prob_0.5" for this work. 

# For now, we are mostly going to be using the results in the "Dens" object to make our maps and then also for cropping with community footprints to get our summaries of change. Let's pull out the density results as a new column
get_dens_func<- function(data){
    out <- data$Dens
    return(out)
}

all_proj_res <- all_proj_res %>%
    mutate(., "Density" = map(Data, get_dens_func))

# What does that get us?
str(all_proj_res$Density[[1]])

# Now, I think you could create a "mapping" function for the surfaces? The "..." is where you might be able to easily add in other things to the plotting function. Relying on some stuff Adam did for FishViz (https://github.com/gulfofmaine/FishVis/blob/master/R/app_support_functions.R)
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

# Now a plotting function...
plot_raw_dens_func<- function(dens_data, baseline_years = seq(from = 2010, to = 2019), projection_years = c(2055, 2075), ...){

    # For debugging, this won't run by itself. Have to go line by line inside the "if"
    if(FALSE){
        dens_data <- all_proj_res$Density[[1]]
        baseline_years <- seq(from = 2010, to = 2019)
        projection_years<- c(2055, 2075)
    }

    # Going to want an "average" across the baseline years..
    base_data <- dens_data %>%
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
    
    # Plot em...
    dens_panel_plot <- ggplot() +
        geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
        geom_sf(data = all_dens_grid, aes(fill = Value, color = Value, geometry = geometry)) +
        scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
        scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
        facet_wrap(~Variable, ncol = 3) +
        coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619)
    
    # Return it
    return(dens_panel_plot)
}

# Let's map that to each of the species...
all_proj_res <- all_proj_res %>%
    mutate(., "Dens_Panel_Plot" = map(Density, plot_raw_dens_func))

# How'd we do?
all_proj_res$Dens_Panel_Plot[[1]]
all_proj_res$Dens_Panel_Plot[[4]]

# ANDREW NOTE: MIGHT WANT TO MASK OUT THE OUTER REACHES OF THE PREDICTED VALUES AS THESE ARE SLIGHTLY BEYOND THE MODELING DOMAIN

###
# Cropping density to community footprints
###
foot_stack <- raster::stack(paste0(project_box_path, "data/All VTR safe fishing footprints by community and gear type 2011-2015.grd"))

# What's in here??
foot_stack

# Dimensions of raster have the number of longitudes (nrow), latitudes (ncol) and then nlayers is the number of unique community and gear type footprints. Generally, we use the "All" combined footprint, which basically kept any unique cell "fished" across the different gear types.
names(foot_stack[[1]])

# Get the stack index for names with "All"
all_ind <- which(grepl("All", names(foot_stack)))
names(foot_stack)[all_ind]

# 126 unique communities. The names are a bit of a nightmare. The easiest thing I think will be to create a table for the selected communities that maps from the community names for the landings to the community names here. I think we have something similar that does this for all the communities...so let me know if you get to that point and you are going nuts!

# Let's reduce the raster stack to just these "All" gear footprints for each of the communities
all_foot_stack<- foot_stack[[all_ind]]

# Plot one?
plot(all_foot_stack[[1]])

# Beautiful :) So, a few things...the "fill" here doesn't really make sense because we are using this "All" gear type. So, we can reclassify this so that we just have binary "fished" and "not-fished" cells.
m <- c(0, Inf, 1,  -Inf, 0, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
all_foot_stack_bin<- raster::reclassify(all_foot_stack, rclmat) 

plot(all_foot_stack_bin[[1]])

# Check that they are in the same spatial projection (gross)
ggplot() +
    geom_tile(data = as.data.frame(all_foot_stack_bin[[1]], xy = TRUE), aes(x = x, y = y)) +
    geom_point(data = all_proj_res$Density[[1]][all_proj_res$Density[[1]]$Time == as.Date("1985-03-16"),], aes(x = Lon, y = Lat), color = "red")

# Now, the overlay. There are a ton of different ways to do this as you probably remember from before. Running out of steam a bit, but happy to help out as you get here!
