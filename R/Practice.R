###Exploratory Mapping
#One RDS file at a time 

lobster_26<-lobster_26$Dens
baseline_years <- seq(from = 2010, to = 2019)
projection_years<- c(2055, 2075)

base_data <- lobster_26 %>%
  filter(., format(Time, "%Y") >= min(baseline_years) & format(Time, "%Y") <= max(baseline_years)) %>%
  group_by(., Lat, Lon) %>%
  summarize(., "Baseline_Mean_Dens" = mean(Prob_0.5)) %>%
  pivot_longer(., -c(Lat, Lon), names_to = "Variable", values_to = "Value")

# Now the future scenarios...
for(i in seq_along(projection_years)){
  fut_data_temp <- lobster_26 %>%
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

all_dens_data <- bind_rows(base_data, fut_data_out)

all_dens_grid <- bSquare(all_dens_data, a = 25000 * 25000, coords = c("Lon", "Lat"))

lob_dens_plot<-all_dens_grid%>%
  filter(Variable == "Baseline_Mean_Dens")%>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = all_dens_grid, aes(fill = Value, color = Value, geometry = geometry)) +
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  theme_gmri()+
  ggtitle("Lobster Baseline (Mean Density)")+
  coord_sf(xlim=c(-80, -55), ylim=c(32,48), crs="+init=epsg:4326") 

##TRANSFORM FOOTPRINTS
foot_test<-all_foot_stack_bin[[1]]
foot_polygon<-rasterToPolygons(foot_test,fun=function(x){x==1})
plot(foot_polygon)
st_bbox(foot_polygon)
footprint<-fortify(foot_polygon)

all_dens_grid%>%
  filter(Variable == "Baseline_Mean_Dens")%>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = all_dens_grid, aes(fill = Value, color = Value, geometry = geometry)) +
  geom_path(data = footprint, aes(x=long, y=lat, group=group))+
  scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
  theme_gmri()+
  ggtitle("Lobster Baseline (Mean Density)")+
  coord_sf(xlim=c(-80, -55), ylim=c(32,48), crs="+init=epsg:4326") 

##sweet baby jesus there is a god 
