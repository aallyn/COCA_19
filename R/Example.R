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

inter_func<-function(data, footprint){
  out<-st_intersection(data, footprint, Value)
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
  mutate(pct_change_2055 = ((Projected_2055_Mean_Dens - Baseline_Means_Dens)/Baseline_Mean_Dens)*100,
         pct_change_2075 = ((Projected_2075_Mean_Dens - Baseline_Means_Dens)/Baseline_Mean_Dens)*100)%>%
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


####Pull different species based on all_dens_grid (ignore footprint component for now)
herring_res <- all_dens_grid %>%
  filter(species_scen == "Herring_full_CMIP6_SSP1_26") %>%
  select(data) %>%
  cbind(portland %>%
          select(PORT, geometry)%>%
          nest(footprint = PORT:geometry))

herring_res %>%
  dplyr::select(points) %>%
  unnest(points)%>%
  select(Variable, Value, geometry) %>%
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
  ggtitle("Herring; Portland, ME") 


herring_res_portland <- herring_res %>%
  reframe(points = map2(data, footprint, inter_func)) %>%
  unnest(points)

herring_res_portland<-herring_res_portland %>% 
  ungroup() %>% 
  distinct() %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(pct_change_2055 = ((Projected_2055_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100,
         pct_change_2075 = ((Projected_2075_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100)

# Herring plots
plot_2055 <- 
  herring_res_portland %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2055, color= pct_change_2055) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  theme_gmri()+
  scale_fill_gradient2(low = "#C80F0F", mid = "#FFFFFF", high = "#EACC1B", na.value = "transparent", midpoint = 0 )+
  scale_color_gradient2(low = "#C80F0F", mid = "#FFFFFF", high = "#EACC1B", na.value = "transparent", midpoint = 0)+
  ggtitle("2055")

plot_2075<-
  herring_res_portland %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  #geom_sf(data = all_lob_dens_grid, aes(geometry=geometry), fill = "#00608A", color = "#00608A") +
  geom_sf(aes( geometry=geometry, fill = pct_change_2075, color= pct_change_2075) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  theme_gmri()+
  scale_fill_gradient2(low = "#C80F0F", mid = "#FFFFFF", high = "#EACC1B", na.value = "transparent" , midpoint = 0)+
  scale_color_gradient2(low = "#C80F0F", mid = "#FFFFFF", high = "#EACC1B", na.value = "transparent" , midpoint = 0)+
  ggtitle("2075")

base_plot <- 
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data =herring_res_portland, aes( geometry=geometry, color= Baseline_Mean_Dens, fill=Baseline_Mean_Dens) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  scale_fill_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 50)+
  scale_color_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 50)+
  theme_gmri()+
  ggtitle("Baseline")

library(gridExtra)
herring_example<-grid.arrange(base_plot, plot_2055, plot_2075, ncol=3)

## Trend plots 
lobster_res <- all_dens_grid %>%
  filter(species_scen == "Lobster_full_CMIP6_SSP1_26") %>%
  select(data) %>%
  cbind(portland %>%
          select(PORT, geometry)%>%
          nest(footprint = PORT:geometry))
lobster_res_portland <- lobster_res %>%
  reframe(points = map2(data, footprint, inter_func)) %>%
  unnest(points)

lobster_res_portland<-lobster_res_portland %>% 
  ungroup() %>% 
  distinct() %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(pct_change_2055 = ((Projected_2055_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100,
         pct_change_2075 = ((Projected_2075_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100)

plot_2055 <- 
  lobster_res_portland %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2055, color= pct_change_2055) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  theme_gmri()+
  scale_fill_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = 20)+
  scale_color_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = 20)+
  ggtitle("2055")

plot_2075<-
  lobster_res_portland %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2075, color= pct_change_2075) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  theme_gmri()+
  scale_fill_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = 30)+
  scale_color_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = 30)+
  ggtitle("2075")

base_plot <- 
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data =lobster_res_portland, aes( geometry=geometry, color= Baseline_Mean_Dens, fill=Baseline_Mean_Dens) , size=5, pch=15)+
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619)+
  scale_fill_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 6000)+
  scale_color_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 6000)+
  theme_gmri()+
  ggtitle("Baseline")

library(gridExtra)
lobster_example<-grid.arrange(base_plot, plot_2055, plot_2075, ncol=3)

# Barplot
barplot<-read_csv("Data/barplot.csv")

ggplot(data=barplot, aes(y=Species, x=Value,  fill=Species, group=Condition,))+
  scale_fill_manual(values=c("#ABB400",
                             "#407331"))+       
  geom_bar(position="dodge",stat="identity", linewidth=0.5, color="white")+
  #geom_text(label= barplot$Condition )+
  theme_gmri()

portland_change<-ggplot(data=barplot) +
  geom_segment( aes(x=0, xend=Value, y=Condition, yend=Condition)) +
  geom_point(aes(x=Value, y=Condition, color=Species),size=3) +
  scale_color_manual(values=c("#ABB400",
                             "#407331"))+
  facet_wrap(~Species, ncol=1, scales = "free_x")+
  xlab("Average Density")+
  ylab(NULL)+
  theme_gmri(legend.position = "none",
             strip.background = element_rect(fill="#00608A"))
ggsave("portland_changes.png", portland_change, height = 5.6, width=3.7, unit="in")

portland_coords <- data.frame("long"= -70.2568, "lat"= 43.6591)
portland_coords <- st_as_sf(portland_coords, coords = c("long", "lat"), crs=4326, remove=FALSE)
portland_coords <- st_transform(portland_coords, crs=32619)

portland_map <- ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = portland, aes(geometry = geometry, group = group), pch=0, size=3, alpha=0.5, color="gray50") + 
  geom_sf(data = portland_coords, aes(geometry = geometry), color = "#00608A", size=3) +
  coord_sf(xlim = c(-182500, 1050000), ylim = c(4275000, 5370000), expand = T, crs = 32619) +
  theme_gmri(plot.margin = (0))

ggsave("portland_map.png", portland_map, height=3.8, width=2.6)

# Re-do Portland landings 
portland_landings <- landings_by_value %>%
 filter(PORT == "PORTLAND, ME")%>%
  unnest(data)%>%
  unnest(data)%>%
  filter(SPPNAME %in% c("LOBSTER, AMERICAN", "HERRING, ATLANTIC", "EEL, AMERICAN")) %>%
  select(SPPNAME, YEAR, VALUE)%>%
  mutate(MIL = (VALUE/1000000))

portland_landings_plot<-ggplot(data = portland_landings, aes(x=YEAR, y=MIL, group=SPPNAME, color=SPPNAME))+
  geom_smooth(se=FALSE)+
  scale_color_manual(values=c("#00608A",
                              "#407331",
                              "#ABB400"))+
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021))+
  theme_gmri()+
  xlab("Year")+
  ylab("$US (Million)")
ggsave("portland_landings_plot.png", portland_landings_plot, height=5, width=7, units = "in")

#Newport News, VA ? 
newportnews <-comm_res%>%
  filter(PORT == "NEWPORT NEWS, VA")%>%
  dplyr::select(fortified)%>%
  unnest(fortified)
newportnews<-st_as_sf(newportnews, coords = c("long", "lat"), crs = 4326, remove=FALSE)
newportnews<-st_transform(newportnews, crs = 32619)
st_bbox(newportnews)

# Newport Scallop
new_scallop <- all_dens_grid %>%
  filter(species_scen == "Scallop_full_CMIP6_SSP1_26") %>%
  select(data) %>%
  cbind(newportnews %>%
          select(PORT, geometry)%>%
          nest(footprint = PORT:geometry))
new_scallop <- new_scallop %>%
  reframe(points = map2(data, footprint, inter_func)) %>%
  unnest(points)

new_scallop <- new_scallop %>% 
  ungroup() %>% 
  distinct() %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(pct_change_2055 = ((Projected_2055_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100,
         pct_change_2075 = ((Projected_2075_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100)

plot_2055 <- 
  new_scallop %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2055, color= pct_change_2055) , size=5, pch=15)+
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = F, crs = 32619)+  theme_gmri()+
  scale_fill_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = 60)+
  scale_color_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = 60)+
  ggtitle("2055")

plot_2075<-
  new_scallop %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2075, color= pct_change_2075) , size=5, pch=15)+
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = F, crs = 32619)+  theme_gmri()+
  scale_fill_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = -60)+
  scale_color_gradient2(low = "#FFFFFF", high = "#F15A22", mid = "#EACC1B", na.value = "transparent" , midpoint = -60)+
  ggtitle("2075")

base_plot <- 
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = new_scallop, aes( geometry=geometry, color= Baseline_Mean_Dens, fill=Baseline_Mean_Dens) , size=5, pch=15)+
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = F, crs = 32619)+  theme_gmri()+
  scale_fill_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 0.4 )+
  scale_color_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 0.4)+
  ggtitle("Baseline")

library(gridExtra)
new_scallop_map<-grid.arrange(base_plot, plot_2055, plot_2075, ncol=3)

# Summer Flounder
new_summer <- all_dens_grid %>%
  filter(species_scen == "Summer_full_CMIP6_SSP1_26") %>%
  select(data) %>%
  cbind(newportnews %>%
          select(PORT, geometry)%>%
          nest(footprint = PORT:geometry))
new_summer <- new_summer %>%
  reframe(points = map2(data, footprint, inter_func)) %>%
  unnest(points)

new_summer <- new_summer %>% 
  ungroup() %>% 
  distinct() %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(pct_change_2055 = ((Projected_2055_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100,
         pct_change_2075 = ((Projected_2075_Mean_Dens - Baseline_Mean_Dens)/Baseline_Mean_Dens)*100)

plot_2055 <- 
  new_summer %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2055, color= pct_change_2055) , size=5, pch=15)+
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = F, crs = 32619)+  theme_gmri()+
  scale_fill_gradient2(mid = "#FFFFFF", high = "#F15A22", low = "#EACC1B", na.value = "transparent" , midpoint = 0)+
  scale_color_gradient2(mid = "#FFFFFF", high = "#F15A22", low = "#EACC1B", na.value = "transparent" , midpoint = 0)+
  ggtitle("2055")

plot_2075<-
  new_summer %>%
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(aes( geometry=geometry, fill = pct_change_2075, color= pct_change_2075) , size=5, pch=15)+
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = F, crs = 32619)+  theme_gmri()+
  scale_fill_gradient2(mid = "#FFFFFF", high = "#F15A22", low = "#EACC1B", na.value = "transparent" , midpoint = 0)+
  scale_color_gradient2(mid = "#FFFFFF", high = "#F15A22", low = "#EACC1B", na.value = "transparent" , midpoint = 0)+
  ggtitle("2075")

base_plot <- 
  ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = new_summer, aes( geometry=geometry, color= Baseline_Mean_Dens, fill=Baseline_Mean_Dens) , size=5, pch=15)+
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = F, crs = 32619)+  theme_gmri()+
  scale_fill_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 0.3 )+
  scale_color_gradient2(low = "#DEECF8", mid = "#009CDE", high = "#236192", na.value = "transparent" , midpoint = 0.3)+
  ggtitle("Baseline")

new_summer_map<-grid.arrange(base_plot, plot_2055, plot_2075, ncol=3)

# Newport News Landings 
newport_news_landings <- landings_by_value %>%
  filter(PORT == "NEWPORT NEWS, VA")%>%
  unnest(data)%>%
  unnest(data)%>%
  filter(SPPNAME %in% c("SCALLOP, SEA", "FLOUNDER, SUMMER", "SEA BASS, BLACK")) %>%
  select(SPPNAME, YEAR, VALUE)%>%
  mutate(MIL = (VALUE/1000000))

newport_news_plot<-ggplot(data = newport_news_landings, aes(x=YEAR, y=MIL, group=SPPNAME, color=SPPNAME))+
  geom_smooth(se=FALSE)+
  scale_color_manual(values=c("#407331",
                              "#ABB400",
                              "#EA4F12"))+
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021))+
  theme_gmri()+
  xlab("Year")+ylab("$US (Million)")
ggsave("newport_news_plot.png", newport_news_plot, height=5, width=7, units = "in")

# Species change plot
newportnews<-read.csv("Data/newportnews.csv")

newport_changes<-ggplot(data=newportnews) +
  geom_segment(aes(x=0, xend=Value, y=Condition, yend=Condition)) +
  geom_point(aes(x=Value, y=Condition, color=Species),size=3) +
  scale_color_manual(values=c("#ABB400",
                              "#407331"))+
  facet_wrap(~Species, ncol=1, scales = "free_x")+
  xlab("Average Density")+
  ylab(NULL)+
  theme_gmri(legend.position = "none",
             strip.background = element_rect(fill="#00608A"))

ggsave("newport_changes.png", newport_changes, height = 5.6, width=3.7, unit="in")

# Newport News Maps
newportnews_coords <- data.frame("long"= -76.4730, "lat"= 37.0871)
newportnews_coords <- st_as_sf(newportnews_coords, coords = c("long", "lat"), crs=4326, remove=FALSE)
newportnews_coords <- st_transform(newportnews_coords, crs=32619)

newportnews_map <- ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = newportnews, aes(geometry = geometry, group = group), pch=0, size=3, alpha=0.5, color="gray50") + 
  geom_sf(data = newportnews_coords, aes(geometry = geometry), color = "#00608A", size=3) +
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = T, crs = 32619)+  theme_gmri()

