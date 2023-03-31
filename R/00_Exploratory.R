library(here)
library(tidyr)
library(tidyverse)
library(readr)
#read in and sort landings data
landings<-read.csv("Data/landings.csv")

landings<-landings%>%
  filter(YEAR > 2012)%>%
  select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE)%>%
  group_by(YEAR, PORT.NAME, STATE)%>%
  nest()


#state landings
maine_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "ME")%>%
  group_by(PORT.NAME)%>%
  nest()

nh_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "NH")%>%
  group_by(PORT.NAME)%>%
  nest()

ma_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "MA")%>%
  group_by(PORT.NAME)%>%
  nest()

ri_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "RI")%>%
  group_by(PORT.NAME)%>%
  nest()

ct_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "CT")%>%
  group_by(PORT.NAME)%>%
  nest()

ny_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "NY")%>%
  group_by(PORT.NAME)%>%
  nest()

nj_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "NJ")%>%
  group_by(PORT.NAME)%>%
  nest()

md_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "MD")%>%
  group_by(PORT.NAME)%>%
  nest()

va_landings<-landings%>%
  unnest(data)%>%
  filter(STATE == "VA")%>%
  group_by(PORT.NAME)%>%
  nest()

##total value per year
test<-nh_landings%>%
  unnest(data)%>%
  filter(PORT.NAME == "HAMPTON")%>%
  group_by(YEAR)%>%
  summarise(sum(parse_number(VALUE))) ##worked. sloppy, but worked. 

##functions
sum_fun<-function(df){
  sum(parse_number(df$VALUE))
}

avg_fun<-function(df){
  mean(parse_number(df$VALUE))
}

avg_2015 <-function(df){
  df<-df%>%
    unnest(data)%>%
    filter(YEAR %in% 2015:2018)
  mean(df$TOTAL, na.rm=TRUE)
}

avg_2019<-function(df){
  df<-df%>%
    unnest(data)%>%
    filter(YEAR %in% 2019:2021)
  mean(df$TOTAL, na.rm=TRUE)
}

test2<-nh_landings%>%
  mutate(TOTAL = map_dbl(data, sum_fun))

test3<-nh_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))
test3<-test3%>%
  select(PORT.NAME, YEAR, TOTAL)%>%
  group_by(PORT.NAME)%>%
  nest() ##success!

#restructure landings and map sum function over
landings<-landings%>%
  unnest(data)
landings_value<-landings%>%
  select(PORT.NAME, YEAR, VALUE, SPPNAME)%>%
  group_by(PORT.NAME, STATE, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun),
         AVG = map_dbl(data, avg_fun))%>%
  group_by(PORT.NAME, STATE)%>%
  nest()

saveRDS(landings_value, "landings_value.RDS")

test6<-landings_value%>%
  mutate(avg2015 = map_dbl(data, avg_2015),
         avg2019 = map_dbl(data, avg_2019))


#map linear model
mod<-function(df){
  lm(TOTAL ~ YEAR, data=df)
}

landings_value<-landings_value%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope =tidy %>% map_dbl(function(x) x$estimate[2]))  ###uh maybe
##do by state####

ma_landsing_test<-ma_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()

ma_landings_test<-ma_landsing_test%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

min(ma_landings_test$slope, na.rm=T)
max(ma_landings_test$slope, na.rm=T)
###works, do this 
ma_landings<-ma_landings_test

maine_landings<-maine_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

maine_landings<-maine_landings%>%
  arrange(desc(slope))

md_landings<-md_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

nh_landings<-nh_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

nj_landings<-nj_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

ny_landings<-ny_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

ri_landings<-ri_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

va_landings<-va_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

ct_landings<-ct_landings%>%
  unnest(data)%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  group_by(PORT.NAME)%>%
  nest()%>%
  mutate(value_mod = map(data, mod),
         tidy = map(value_mod, broom::tidy),
         slope = tidy %>% map_dbl(function(x) x$estimate[2]))

min_max<-function(df){
  x<-min(df$slope, na.rm=T)
  y<-max(df$slope, na.rm=T) 
  print(x)
  print(y)
}
min_max(maine_landings)
min_max(ma_landings)
min_max(nh_landings)
min_max(ct_landings)
min_max(ri_landings)
min_max(md_landings)
min_max(nj_landings)
min_max(ny_landings)
min_max(va_landings)

ma_landings_test<-ma_landings_test%>%
  arrange(desc(slope))
nh_landings<-nh_landings%>%
  arrange(desc(slope))
ct_landings<-ct_landings%>%
  arrange(desc(slope))
ri_landings<-ri_landings%>%
  arrange(desc(slope))
nj_landings<-nj_landings%>%
  arrange(desc(slope))
ny_landings<-ny_landings%>%
  arrange(desc(slope))
md_landings<-md_landings%>%
  arrange(desc(slope))
va_landings<-va_landings%>%
  arrange(desc(slope))

maine <- maine_landings%>%
  filter(PORT.NAME %in% c("WALDO", "ORRINGTON", "CHAMBERLAIN", "ISLE AU HAUT", "BUCKSPORT"))
nh <- nh_landings%>%
  filter(PORT.NAME == "HAMPTON")
ma<- ma_landings%>%
  filter(PORT.NAME %in% c("BERKLEY", "WEYMOUTH", "AQUINNAH", "WEST TISBURY", "SWANSEA"))
ct<-ct_landings%>%
  filter(PORT.NAME %in% c("NOANK", "NORWALK", "CLINTON", "NEW LONDON", "STONINGTON"))
ri<-ri_landings%>%
  filter(PORT.NAME %in% c("NEW SHOREHAM", "NARRAGANSETT", "BRISTOL", "EAST GREENWICH", "NEWPORT"))
ny<-ny_landings%>%
  filter(PORT.NAME %in% c("QUEENS", "SOUTHAMPTON", "EASTPORT", "STONY BROOK", "HUNTINGTON"))
nj<-nj_landings %>%
  filter(PORT.NAME %in% c("ATLANTIC", "WARETOWN", "BELMAR", "BRIELLE", "BELFORD"))
md<-md_landings %>%
  filter(PORT.NAME %in% c("CHARLES", "BALTIMORE", "HOWARD", "ST. MARY'S", "KENT"))
va<-va_landings%>%
  filter(PORT.NAME %in% c("GREENBACKVILLE", "STAFFORD", "ESSEX", "JAMES CITY", "KING & QUEEN"))

coca_comm<-bind_rows(maine, nh, ma, ct, ri, ny, nj, md, va)
coca_comm_clean<-coca_comm%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, slope)%>%
  distinct()

coca_comm_value<-coca_comm%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, YEAR, TOTAL)%>%
  distinct()

write.csv(coca_comm_value, "COCA_communities_total_value.csv")

coca_comm<-coca_comm%>%
  mutate(glance = map(value_mod, broom::glance),
         augment = map(value_mod, broom::augment),
         p = glance %>% map_dbl("p.value"))

coca_comm_clean<-coca_comm_clean%>%
  left_join(coca_comm%>%
              select(p))

write.csv(coca_comm_clean,"COCA_communities.csv")

#plots
coca_comm_value%>%
  filter(STATE == "ME")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("Maine Communities")

coca_comm_value%>%
  filter(STATE == "NH")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("New Hampshire Communities")

coca_comm_value%>%
  filter(STATE == "MA")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("Massachusetts Communities")

coca_comm_value%>%
  filter(STATE == "CT")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("Connecticut Communities")

coca_comm_value%>%
  filter(STATE == "RI")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("Rhode Island Communities")

coca_comm_value%>%
  filter(STATE == "NY")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("New York Communities")

coca_comm_value%>%
  filter(STATE == "MD")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  ggtitle("Maryland Communities")

coca_comm_value%>%
  filter(STATE == "VA")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=3)+
  theme_gmri(axis.text.y = element_text(size = 9))+
  scale_y_continuous(breaks= c(10000, 100000, 300000))+
  ggtitle("Virginia Communities")

coca_comm_value%>%
  filter(STATE == "NJ")%>%
  ggplot(aes(YEAR, TOTAL))+
  geom_point()+
  geom_line()+
  facet_wrap(~PORT.NAME, ncol=2)+
  theme_gmri()+
  scale_y_continuous(breaks = c(20000, 3000000))+
  ggtitle("New Jersey Communities")

##plot each port individually
comm_df<-coca_comm_value%>%
  select(PORT.NAME, STATE, YEAR, TOTAL)%>%
  group_by(PORT.NAME)%>%
  nest()
comm_plots<-vector("list", length = 41)
names(comm_plots)=paste(unique(coca_comm_value$PORT.NAME))

for(i in 1:41){
  loop_df<-comm_df[i,]%>%
    unnest(data)%>%
    select(PORT.NAME, STATE, YEAR, TOTAL)%>%
    group_by(PORT.NAME)%>%
    distinct()

  comm_plots[[i]]<-ggplot(data = loop_df, aes(YEAR, TOTAL))+
    geom_point()+
    geom_line()+
    theme_gmri()+
    ggtitle(names(comm_plots[i]))+
    ylab("Total $")+
    xlab("Year")
  
  filename=paste(unique(loop_df$PORT.NAME), unique(loop_df$STATE, sep="_"))
  ggsave(comm_plots[[i]], file=paste(filename, ".pdf", sep=""),
         width = 7, height= 5)
}
comm_list<-comm_plots[26]
print(comm_list)

###p plot next
coca_comm_clean%>%
  arrange(desc(slope))

coca_comm_clean%>%
  drop_na()%>%
  mutate(signif = p<0.05)%>%
  ggplot()+
  geom_point(aes(x=PORT.NAME, y= p, color=as.factor(signif)))+
  geom_text_repel(aes(PORT.NAME, p, label=PORT.NAME), size=2.8, nudge_y=0.003)+
  theme_gmri(axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             legend.position="none")+
  scale_color_gmri()+
  geom_hline(yintercept=0.05, linetype=2, linewidth=0.5, color="#00736D")+
  ylab("Significance")+
  ggtitle("Significant Decreases in Catch Value")

###add p.value to overall landings data
landings_value<-landings_value%>%
  mutate(
    glance = map(value_mod, broom::glance),
    p = glance %>% map_dbl("p.value"),
    sign = p<0.05)

decreasing<-landings_value%>%
  filter(sign == "TRUE")%>%
  filter(slope < 0)%>%
  arrange(desc(slope))

increasing<-landings_value%>%
  filter(sign == "TRUE")%>%
  filter(slope > 0)%>%
  arrange(slope)

decreasing_clean<-decreasing%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, slope, p)%>%
  distinct()%>%
  arrange(PORT.NAME)

increasing_clean<-increasing%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, slope, p)%>%
  distinct()%>%
  arrange(PORT.NAME)

write.csv(decreasing_clean, here("Data", "COCA_communities_decreasing.csv"))
write.csv(increasing_clean, here("Data", "COCA_communities_increasing.csv"))

#Kathy's revisions####
coca_comm_unnested<-coca_comm%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, YEAR, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE)

write.csv(coca_comm_unnested, "COCA_communities_GARFO.csv")

high_value_comm<-test6
high_val_2015<-high_value_comm%>%
  select(PORT.NAME, STATE, data, avg2015)%>%
  arrange(desc(avg2015))
high_val_2019<-high_value_comm%>%
  select(PORT.NAME, STATE, data, avg2019)%>%
  arrange(desc(avg2019))

test7<-high_val_2019%>%
  group_by(avg2019)%>%
  rowid_to_column()%>%
  filter(rowid %in% seq(1,100)) #wheeeeeeeee

high_val_2015<-high_val_2015%>%
  rowid_to_column()%>%
  filter(rowid %in% seq(1,100))

high_val_2019<-high_val_2019%>%
  rowid_to_column()%>%
  filter(rowid %in% seq(1,100))

clean2015<-high_val_2015%>%
  select(PORT.NAME, STATE, avg2015)
clean2019<-high_val_2019%>%
  select(PORT.NAME, avg2019)
write.csv(clean2015, "High_value_2015_18.csv")
write.csv(clean2019, "High_value_2019.csv")
         
unnested_2015<-high_val_2015%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, YEAR, SPPNAME, VALUE)

unnested_2019<-high_val_2019%>%
  unnest(data)%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, YEAR, SPPNAME, VALUE)
write.csv(unnested_2015, "unnested_2015.csv")
write.csv(unnested_2019, "unnested_2019.csv")

##Carly's Revisions
mean.function<-function(df){
  df<-df%>%
    unnest(data)%>%
    filter(YEAR %in% 2015:2018)
  mean(df$TOTAL, na.rm=TRUE)
}

high_value<-landings_value%>%
  mutate(average_value = map_dbl(data, mean.function))%>%
  select(PORT.NAME, STATE, average_value)%>%
  arrange(desc(average_value))%>%
  rowid_to_column()%>%
  filter(rowid %in% seq(1,100))

write.csv(high_value, here("Data", "high_value_communities_10_years.csv"))

##report example####
example<-landings%>%
  filter(PORT.NAME == "BELMAR")%>%
  group_by(PORT.NAME, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun),
         TOT_THOUS = TOTAL/1000)

gmri_cols()

ggplot(example, aes(YEAR, TOT_THOUS))+
  geom_area(fill="#E9E9E9")+
  geom_line(color = "#535353")+
  theme_gmri(
    plot.title = element_text(color = "#00608A"),
    axis.title.x = element_text(color = "#00608A"),
    axis.title.y = element_text(color = "#00608A")
  )+
  scale_y_continuous(breaks = seq(60, 500, by=50))+
  ggtitle("Total Landed Value")+
  xlab("Year")+
  ylab("US$ (thousands)")

world <- ne_countries(scale = "medium", returnclass = "sf")
coast<-ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim=c(-76, -72), ylim=c(38,42))

port_map<-ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(aes(x=-70.2568, y=43.6591), color = "#00608A", size = 3.5)+
  theme_gmri(
    plot.background = element_blank(),
    plot.title = element_text(color = "#00608A"),
    axis.title.x = element_text(color = "#00608A"),
    axis.title.y = element_text(color = "#00608A"))+
  coord_sf(xlim=c(-72, -66), ylim=c(41,46), expand=TRUE) +
  scale_x_continuous(breaks=c(-72, -70, -68))+scale_y_continuous(breaks = c(40, 42, 44, 46))+
  xlab("Longitude") +
  ylab("Latitude")
ggsave("portland_map.png", port_map, height=4, width=3, unit="in")
###EXAMPLE DECADAL PLOTS (require clean_survey)
clean_survey%>%
  filter(comname == "american lobster")

belmar<-clean_survey%>%
  filter(comname %in% c("jonah crab", "american lobster" ))%>%
  group_by(comname,est_year,season)%>%
  summarise(COG=COGravity(x=decdeg_beglon, y=decdeg_beglat, z=NULL, wt=biomass_kg))%>%
  unnest_longer(COG)%>%
  pivot_wider(names_from=COG_id, values_from = COG)%>%
  select("comname","est_year", "season", "COGx", "COGy")%>%
  relocate(COGx, .after=COGy)

belmar<-belmar%>%
  mutate(decade = 10*est_year %/% 10)%>%
  group_by(comname)%>%
  nest()

belmar_lob<-belmar%>%
  unnest(data)%>%
  filter(comname == "american lobster")
belmar_crab<-belmar%>%
  unnest(data)%>%
  filter(comname == "jonah crab")
ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(36,47))+
  geom_point(data= belmar_crab, aes(x=COGx,y=COGy,color=season))+
  geom_point(aes(x=-74.034, y=40.18), color = "#00736D", size = 1)+
  theme_gmri(axis.text.x = element_text(size = 9))+
  ggtitle("Jonah Crab")+
  ylab("Center of Latitude")+
  xlab("Center of Longitude")+
  scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-76,-70,-64)) +
  facet_wrap(~decade, ncol=5)

gmri_cols()
###landed value
example<-example%>%
  unnest(data)%>%
  filter(SPPNAME %in% c("CRAB, JONAH", "LOBSTER, AMERICAN"))%>%
  mutate(live_lbs = parse_number(LIVE.LBS))

ggplot(example, aes(YEAR, live_lbs))+
  geom_area(fill = "#ABB400")+
  geom_line(color = "#3B4620", linewidth = 1)+
  theme_gmri(panel.background = element_rect(fill = "#E9E9E9"))+
  xlab("Year")+
  ylab("Pounds")+
  facet_wrap(~SPPNAME, nrow=2, scales = "free_y")
 
#scale_y_continuous(breaks = seq(2000, 80000, by=10000))
##proportions graphic
portland<-landings_value%>%
  filter(PORT.NAME == "PORTLAND")%>%
  select(PORT.NAME, data)%>%
  unnest(data)%>%
  select(!TOTAL)%>%
  select(!AVG)%>%
  unnest(data)%>%
  group_by(SPPNAME)%>%
  nest()%>%
  mutate(AVG =  map(data, avg_fun))%>%
  mutate(AVG = as.numeric(AVG))

prct<-function(x){
  (x/sum(portland$AVG))*100
}

portland_prop<-portland%>%
  mutate(percent = map(AVG, prct))%>%
  mutate(percent = as.numeric(percent))

#treemap
install.packages("treemap")
library(treemap)

treemap(portland,
        
        # data
        index=c("SPPNAME", "AVG"),
        vSize="AVG",
        type="index"
      
) 
#stacked area
pwm<-portland%>%
  unnest(data)%>%
  select(SPPNAME, YEAR, VALUE)%>%
  group_by(SPPNAME)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, sum_fun))%>%
  mutate(TOTAL = as.numeric(TOTAL))%>%
  arrange(desc(TOTAL))%>%
  rowid_to_column()%>%
  filter(rowid %in% seq(1,5))

pwm<-pwm%>%
  unnest(data)%>%
  select(SPPNAME, YEAR, VALUE)%>%
  mutate(VALUE = parse_number(VALUE),
         MIL = (VALUE/1000000))

pwm_plot<-ggplot(data = pwm, aes(x=YEAR, y=MIL, group=SPPNAME, color=SPPNAME))+
  geom_smooth(se=FALSE)+
  scale_color_gmri()+
  theme_gmri()+
  xlab("Year")+ylab("$US (Million)")

ggsave("portland_landings.png", pwm_plot, height=5, width=7, units = "in")
  
#port map
library(leaflet)
portland_map<-leaflet(data=pwm) %>% 
  addProviderTiles(provider="Esri.OceanBasemap") %>%
  setView(lng= -70.25682, lat=43.65910, zoom=7) %>%
  addCircleMarkers(~-70.2568, ~43.6591, radius=10, color="#00608A")

##bar chart..?
barplot<-read_csv("barplot.csv")

barplot<-barplot%>%
  arrange(desc(Value))

barplot<-ggplot(data=barplot, aes(y=Species, x=Value,  fill=Species, group=Condition,))+
  scale_fill_manual(values=c("#00608A",
                             "#ABB400",
                             "#407331",
                             "#EA4F12",
                             "#EACA00"))+       
  geom_bar(position="dodge",stat="identity", linewidth=0.5, color="white")+
  theme_gmri(legend.position = "none",
             axis.title.y = element_blank(),
             axis.text.y = element_blank())+
  xlab("Percent")
print(barplot)
ggsave("changes_plot.png", barplot, height=5, width=4, units="in")  
