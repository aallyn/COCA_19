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
  filter(PORT.NAME %in% c("NEWPORT", "NORTH KINGSTOWN", "MENEMSHA", "CHATHAM", "PROVINCETOWN"))
