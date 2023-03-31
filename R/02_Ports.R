#Read in landings data
landings_value<-readRDS("Data/landings_value.RDS")

kathy_revisions<-c("FALMOUTH", "PROVINCETOWN", "MENEMSHA", "BOOTHBAY", "ST. GEORGE", "YORK", "WELLS", "KENNEBUNK", 
                   "KENNEBUNKPORT","NEWPORT", "NORTH KINGSTOWN", "CHATHAM")
kathy_landings<-landings_value%>%
  filter(PORT.NAME %in% kathy_revisions)%>%
  filter(!STATE == "VA")%>%
  unnest(data)%>%
  unnest(data)

york<-kathy_landings%>%
  filter(PORT.NAME == "YORK")%>%
  filter(!is.na(AVG))%>%
  filter(!SPPNAME == "CONFIDENTIAL")

kathy_landings<-kathy_landings%>%
  filter(!PORT.NAME == "YORK")%>%
  rbind(york)%>%
  group_by(PORT.NAME, STATE)%>%
  nest(SPECIES = c(SPPNAME, VALUE))%>%
  group_by(PORT.NAME, STATE)%>%
  nest()%>%
  mutate(TOTAL_AVG = map(data, possibly((function(df){mean(df$AVG)}), NA)))

species<-kathy_landings%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, YEAR, SPECIES)%>%
  group_by(PORT.NAME, STATE)%>%
  nest()

kathy_landings<-kathy_landings%>%
  left_join(species)


########################################################
species_comp<-kathy_landings%>%
  filter(PORT.NAME %in% c("NEWPORT", "NORTH KINGSTOWN", "MENEMSHA", "CHATHAM", "PROVINCETOWN"))%>%
  unnest(data)%>%
  select(PORT.NAME, STATE, YEAR, SPECIES)%>%
  unnest(SPECIES)%>%
  mutate(VALUE = parse_number(VALUE))%>%
  group_by(PORT.NAME, STATE, YEAR)%>%
  nest()

top_species<-function(df){
  temp_df <- df%>%
    arrange(desc(VALUE))%>%
    rowid_to_column()%>%
    filter(rowid %in% seq(1,5))
}

chatham<-species_comp%>%
  filter(PORT.NAME == "CHATHAM")%>%
  mutate(TOP_SPP = map(data, top_species))

chatham%>%
  unnest(TOP_SPP)%>%
  select(YEAR, SPPNAME, VALUE)%>%
  mutate(MIL = (VALUE/1000000))%>%
  ggplot(aes(x=YEAR, y=MIL, fill=SPPNAME, group=SPPNAME))+
  geom_bar(stat="identity")+
  theme_gmri()+
  scale_fill_gmri()+
  scale_x_continuous(breaks=c(2013, 2015, 2017, 2019, 2021))+
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25))

#plot loop for species comp
comp_df<-species_comp%>%
  mutate(TOP_SPP = map(data, top_species))%>%
  unnest(TOP_SPP)%>%
  select(PORT.NAME, STATE, YEAR, VALUE, SPPNAME)%>%
  group_by(PORT.NAME)%>%
  nest()
comp_plots<-vector("list",length = 5)
names(comp_plots)=paste(unique(comp_df$PORT.NAME))

for(i in 1:5){
  print(i)
  loop_df<-comp_df[i,]%>%
    unnest(data)%>%
    mutate(MIL = (VALUE/1000000))
  
 comp_plots[[i]] <- ggplot(data = loop_df, aes(x=YEAR, y=MIL, fill=SPPNAME, group=SPPNAME))+
    geom_bar(stat="identity", linewidth=0.5, color="white")+
    #geom_label_repel(aes(label=SPPNAME, position="identity", stat="identity",
                    #max.overlaps = getOption("ggrepel.max.overlaps", default = 20),
                     #min.segment.length = 5)+
    theme_gmri()+
    scale_fill_gmri()+
    scale_x_continuous(breaks=c(2013, 2015, 2017, 2019, 2021))+
    ylab("USD (millions)")+
    xlab("Year")+
    ggtitle(names(comp_plots)[i])
}
print(comp_plots[1])
species_comp_plots<-marrangeGrob(comp_plots, layout_matrix = matrix(1:2, nrow=2, ncol=2), top=NULL)
ggsave("species_comp_plots.pdf", species_comp_plots, height=9, width=16, units = "in")

kathy_clean<-kathy_landings%>%
  select(PORT.NAME, STATE, TOTAL_AVG)%>%
  mutate(TOTAL_AVG = as.numeric(TOTAL_AVG))
write.csv(kathy_clean, "revised_landings.csv")
