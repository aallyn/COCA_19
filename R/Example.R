##Example Workflow for report mockup
#
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
portland<-comm_res%>%
  filter(PORT == "PORTLAND, ME")%>%
  dplyr::select(fortified)%>%
  unnest(fortified)
portland<-st_as_sf(portland, coords = c("long", "lat"), crs = 4326, remove=FALSE)
portland<-st_transform(portland, crs = 32619)

# Plot em..
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

density <- test_data %>%
  dplyr::select(Variable, points)%>%
  unnest(points)%>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes(fill = Value, color = Value, geometry = geometry), size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  scale_fill_gradientn(colors = c("#DEECF8","#009CDE", "#236192", "#011854", "black"), na.value = "transparent")+
  scale_color_gradientn(colors = c("#DEECF8","#009CDE", "#236192", "#011854", "black"), na.value = "transparent")+
  theme_gmri(strip.background = element_rect(fill = "#00608A"),
             legend.text = element_text(size = 9, angle = 45),
             legend.title = element_text(size = 9, vjust = 1))+
  facet_wrap(~Variable)+
  ggtitle("Lobster; Portland, ME") 
#meh

ggsave("lobster_density.png", density, height=5, width=7, unit="in")

# Calculate Percent Change
test_2055<-test_data%>%
  filter(Variable == "Baseline_Mean_Dens")%>%
  ungroup(Variable)%>%
  select(points)%>%
  unnest(points)%>%
  select(Value)%>%
  rename("Baseline_Value" = "Value")%>%
  cbind(test_data%>%
          filter(Variable == "Projected_2055_Mean_Dens")%>%
          ungroup(Variable)%>%
          select(points)%>%
          unnest(points)%>%
          select(Value)%>%
          rename("Projected_Value" = "Value"))%>%
  reframe(pct_change = ((Baseline_Value - Projected_Value)/Baseline_Value)*100)%>%
  mutate(z = pct_change > 0)%>%
  cbind(test_data%>%
          filter(Variable == "Projected_2055_Mean_Dens")%>%select(points)%>%unnest(points)%>%select(geometry))

ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = test_2055, aes( geometry=geometry, color= pct_change) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = F, crs = 32619)+
  scale_color_gmri(discrete = F)+
  theme_gmri()+
  ggtitle("Lobster; Portland, ME")

#2075
test_2075<-test_data%>%
  filter(Variable == "Baseline_Mean_Dens")%>%
  ungroup(Variable)%>%
  select(points)%>%
  unnest(points)%>%
  select(Value)%>%
  rename("Baseline_Value" = "Value")%>%
  cbind(test_data%>%
          filter(Variable == "Projected_2075_Mean_Dens")%>%
          ungroup(Variable)%>%
          select(points)%>%
          unnest(points)%>%
          select(Value)%>%
          rename("Projected_Value" = "Value"))%>%
  reframe(pct_change = ((Baseline_Value - Projected_Value)/Baseline_Value)*100)%>%
  mutate(z = pct_change > 0)%>%
  cbind(test_data%>%
          filter(Variable == "Projected_2075_Mean_Dens")%>%select(points)%>%unnest(points)%>%select(geometry))
ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = test_2075, aes( geometry=geometry, color= pct_change) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = F, crs = 32619)+
  scale_color_gmri(discrete = F)+
  theme_gmri()+
  ggtitle("Lobster; Portland, ME")

#combine them? 
percent_change <- test_2055 %>% rbind(test_2075)
ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = percent_change, aes( geometry=geometry, color= pct_change) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = F, crs = 32619)+
  scale_color_gmri(discrete = F)+
  facet_wrap(~Variable)+
  theme_gmri()+
  ggtitle("Lobster SSP1-2.6; Portland, ME")

# Try with all_lob_dens_grid
all_lob_dens_grid<-all_lob_dens_grid%>%
  pivot_wider(names_from = "Variable", values_from = "Value")%>%
  mutate(pct_change_2055 = ((Baseline_Mean_Dens - Projected_2055_Mean_Dens)/Baseline_Mean_Dens)*100,
         pct_change_2075 = ((Baseline_Mean_Dens - Projected_2075_Mean_Dens)/Baseline_Mean_Dens)*100)%>%
  #pivot_longer(cols=4:6, names_to="Variable", values_to="Value")%>%
  select(!Lat)%>%
  select(!Lon)

# Mock up report plots
install.packages("devtools")
devtools::install_github("kevinsblake/NatParksPalettes")
library(NatParksPalettes)
names(NatParksPalettes)

plot_2055 <- 
  percent_change %>%
  filter(Variable == "Projected_2055_Mean_Dens") %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change, color= pct_change) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  theme_gmri()+
  scale_fill_gradient2(low = "#C80F0F", mid = "#F15A22", high = "#EACC1B", na.value = "transparent" , midpoint = -30)+
  scale_color_gradient2(low = "#C80F0F", mid = "#F15A22", high = "#EACC1B", na.value = "transparent" , midpoint = -30)+
  #scale_fill_natparks_c("Acadia", na.value = "transparent" , midpoint = -30)+
  #scale_color_natparks_c("Acadia", na.value = "transparent" , midpoint = -30)+
  ggtitle("2055")

plot_2075<-
  percent_change %>%
  filter(Variable == "Projected_2075_Mean_Dens") %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  #geom_sf(data = all_lob_dens_grid, aes(geometry=geometry), fill = "#00608A", color = "#00608A") +
  geom_sf(aes( geometry=geometry, fill = pct_change, color= pct_change) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  theme_gmri()+
  scale_fill_gradient2(low = "#C80F0F", mid = "#F15A22", high = "#EACC1B", na.value = "transparent" , midpoint = -30)+
  scale_color_gradient2(low = "#C80F0F", mid = "#F15A22", high = "#EACC1B", na.value = "transparent" , midpoint = -30)+
  ggtitle("2075")

base_plot <- 
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  #geom_sf(data = all_lob_dens_grid, aes(geometry=geometry, color = Baseline_Mean_Dens, fill = Baseline_Mean_Dens), alpha = 0.6) +
  geom_sf(data =base_test, aes( geometry=geometry, color= Value, fill=Value) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  scale_fill_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 6000)+
  scale_color_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 6000)+
  theme_gmri()+
  ggtitle("Baseline")

library(gridExtra)
lobster_example<-grid.arrange(base_plot, plot_2055, plot_2075, ncol=3)
ggsave("lobster_change.png", lobster_example, width=1657, height = 608, units="px") #this isn't right 
