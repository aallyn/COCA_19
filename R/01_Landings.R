##LANDINGS FILTERING v2
landings<-read.csv("Data/landings.csv")
landings<-landings%>%
  filter(YEAR > 2012)%>%
  select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE)%>%
  filter(!is.na(VALUE))%>%
  mutate(VALUE = parse_number(VALUE))%>%
  group_by(PORT.NAME, STATE, YEAR)%>%
  nest()%>%
  mutate(TOTAL = map_dbl(data, (function(df){sum(df$VALUE, na.rm=TRUE)})))

landings_by_value<-landings%>%
  group_by(PORT.NAME, STATE)%>%
  nest()%>%
  mutate(AVERAGE_TOTAL = map_dbl(data, (function(df){mean(df$TOTAL, na.rm=TRUE)})))%>%
  arrange(desc(AVERAGE_TOTAL))%>% 
  rowid_to_column()%>%
  filter(rowid %in% seq(1,100))%>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ")
View(landings_by_value)
write.csv((landings_by_value%>%
             select(PORT, AVERAGE_TOTAL)), "landings_by_value_2013.csv")

##top landings plots
landings_plots<-vector("list", length=100)
names(landings_plots)=paste(unique(landings_by_value$PORT))

for(i in 1:100){
  print(i)
  
  loop_df<-landings_by_value[i,]%>%
    unnest(data)%>%
    select(PORT, YEAR, TOTAL)%>%
    mutate(MIL = TOTAL/1000000)
  
  landings_plots[[i]]<-ggplot(data = loop_df, aes(x=YEAR, y=MIL))+
    geom_point()+
    geom_line(color = "#00608A")+
    theme_gmri(plot.title = element_text(size=10))+
    scale_x_continuous(breaks=c(2013, 2015, 2017, 2019, 2021))+
    ylab("USD (millions)")+
    xlab("Year")+
    ggtitle(names(landings_plots)[i])
}

print(landings_plots[62])
landings_plot_list<-marrangeGrob(landings_plots, layout_matrix = matrix(1:15, nrow=5, ncol=3), top=NULL)
ggsave("landings_plots.pdf", landings_plot_list, height=11, width=8.5, units = "in")

###species proportions
species_prp<-landings%>%
  filter(YEAR>2016)%>%
  unnest(data)%>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ")%>%
  select(PORT, YEAR, SPPNAME, VALUE)%>%
  #mutate(PER.RANK = percent_rank(VALUE))%>%
  group_by(PORT)%>%
  nest()

ports<-read.csv("Data/ports_proposed.csv")%>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ")

rank<-function(df){
  out<-df%>%
    mutate(PER.RANK = percent_rank(SUM))
  return(out)
}

port_species<-species_prp%>%
  filter(PORT %in% ports$PORT)%>%
  unnest(data)%>%
  group_by(PORT, SPPNAME)%>%
  nest()%>%
  mutate(SUM = map_dbl(data, function(df){sum(df$VALUE)}))%>%
  select(!data)%>%
  group_by(PORT)%>%
  nest()%>%
  mutate(PER.RANK = map(data, rank))%>%
  select(!data)%>%
  unnest(PER.RANK)

##top species per ports (>90%)
top_species<-port_species%>%
  filter(PER.RANK > 0.9)%>%
  select(!SUM)
write.csv(top_species, "top_species_by_port.csv")
