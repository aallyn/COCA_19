##Example plots for report mock-up
#
# Bring in example (using the footprints separate from the .RDS file for now)
comm_res <- readRDS("Data/community_footprint_results.RDS")
portland<-comm_res%>%
  filter(PORT == "PORTLAND, ME")%>%
  dplyr::select(fortified)%>%
  unnest(fortified)
portland<-st_as_sf(portland, coords = c("long", "lat"), crs = 4326, remove=FALSE)
portland<-st_transform(portland, crs = 32619)

####Pull different species based on all_dens_grid (ignore footprint component for now)
all_dens_grid <- readRDS("Data/all_dens_footprinds.RDS") #accidental typo in the file name

# Herring for Portland, ME
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

# Function for st_intersection to crop density outputs to community
function(data, footprint){
  out<-st_intersection(data, footprint, Value)
  print(out)
}

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

## Lobster for Portland, ME 
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

# Barplot/Species Changes
barplot<-read_csv("Data/barplot.csv") #This was made manually for my own sake - it contains the means of each variable (baseline, 2055, 2075)

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

# Portland landings 
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

# Newport News, VA ? 
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

# Newport News Port Map
newportnews_coords <- data.frame("long"= -76.4730, "lat"= 37.0871)
newportnews_coords <- st_as_sf(newportnews_coords, coords = c("long", "lat"), crs=4326, remove=FALSE)
newportnews_coords <- st_transform(newportnews_coords, crs=32619)

newportnews_map <- ggplot()+
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  geom_sf(data = newportnews, aes(geometry = geometry, group = group), pch=0, size=3, alpha=0.5, color="gray50") + 
  geom_sf(data = newportnews_coords, aes(geometry = geometry), color = "#00608A", size=3) +
  coord_sf(xlim = c(-210000, 740000), ylim = c(3800000, 4800000), expand = T, crs = 32619)+  theme_gmri()

