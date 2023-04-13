##Filter all_proj_res to one species scenario
lobster_res<-all_proj_res%>%
  filter(Species_Scenario == "Lobster_full_CMIP6_SSP1_26")
lobster_res<-lobster_res%>%
  mutate(., "Density" = map(Data, get_dens_func))
names(lobster_res$Density[[1]])
lobster_res$Species_Scenario[1]

lob_dens_data <- lobster_res$Density[[1]]
species_scen <- lobster_res$Species_Scenario[[1]]
baseline_years <- seq(from = 2010, to = 2019)
projection_years<- c(2055, 2075)

# Going to want an "average" across the baseline years..
base_data <- lob_dens_data %>%
  filter(., format(Time, "%Y") >= min(baseline_years) & format(Time, "%Y") <= max(baseline_years)) %>%
  group_by(., Lat, Lon) %>%
  summarize(., "Baseline_Mean_Dens" = mean(Prob_0.5)) %>%
  pivot_longer(., -c(Lat, Lon), names_to = "Variable", values_to = "Value")
  
# Now the future scenarios...
for(i in seq_along(projection_years)){
  fut_data_temp <- lob_dens_data %>%
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
all_lob_dens_data <- bind_rows(base_data, fut_data_out)
  
# Create "grid cells"
all_lob_dens_grid <- bSquare(all_lob_dens_data, a = 25000 * 25000, coords = c("Lon", "Lat"))
crs(all_lob_dens_grid)

# Bring in example 
beals<-comm_res%>%
  filter(PORT == "BEALS, ME")%>%
  dplyr::select(fortified)%>%
  unnest(fortified)
beals<-st_as_sf(beals, coords = c("long", "lat"), crs = 4326, remove=FALSE)
beals<-st_transform(beals, crs = 32619)
st_crs(beals) == st_crs(all_lob_dens_grid$geometry)

# Another example
portland<-comm_res%>%
  filter(PORT == "PORTLAND, ME")%>%
  dplyr::select(fortified)%>%
  unnest(fortified)
portland<-st_as_sf(portland, coords = c("long", "lat"), crs = 4326, remove=FALSE)
portland<-st_transform(portland, crs = 32619)

# Plot em...
ggplot() +
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = all_lob_dens_grid, aes(fill = Value, color = Value, geometry = geometry)) +
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  geom_path(data = beals, aes(x=long, y=lat, group=group))+  
  theme_gmri()+
  ggtitle(species_scen)+
  facet_wrap(~Variable, ncol = 3) +
  coord_sf(xlim=c(-74, -66), ylim=c(42,48), crs="+init=epsg:4326") 

ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = st_intersection(all_lob_dens_grid, beals), aes(fill=Value, color=Value), pch=15)+
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  theme_gmri()+
  ggtitle(species_scen)+
  facet_wrap(~Variable, ncol = 3) +
  coord_sf(xlim=c(-74, -66), ylim=c(42,48), crs="+init=epsg:4326") 

ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15)+
  geom_sf(data = st_intersection(all_lob_dens_grid, portland), aes(fill=Value, color=Value), size = 3, pch=15)+
  #geom_path(data = portland, aes(x=long, y=lat, group=group))+
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  theme_gmri()+
  ggtitle(species_scen)+
  facet_wrap(~Variable, ncol = 3) +
  coord_sf(xlim=c(-74, -67), ylim=c(40,47), crs="+init=epsg:4326") 

# Map st_intersection over scenario & port? 
test_data<-all_lob_dens_grid %>%
  dplyr::select(Variable, geometry, Value) %>%
  group_by(Variable)%>%
  nest(grid = geometry:Value) %>%
  cbind(portland %>%
          dplyr::select(PORT, geometry) %>%
          nest(footprint = PORT:geometry))

inter_func<-function(grid, footprint){
  out<-st_intersection(grid, footprint, Value)
  print(out)
}
test_data<-test_data%>%
  mutate(points = map2(grid, footprint, inter_func))
# Okay that kinda worked...
base_test<-test_data%>%
  filter(Variable == "Baseline_Mean_Dens")%>%
  dplyr::select(Variable, points)%>%
  unnest(points) 
test_data%>%
  dplyr::select(Variable, points)%>%
  unnest(points)%>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes(fill = Value, color = Value, geometry = geometry), size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = F, crs = 32619)+
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  theme_gmri()+
  facet_wrap(~Variable)+
  ggtitle("Lobster; Portland, ME")


# Pause on this for a minute
base_test<-st_as_sf(base_test)
base_test<-as_Spatial(base_test)
plot(base_test) # almost
base_test<-raster(base_test)
plot(base_test)
