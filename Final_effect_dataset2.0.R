

aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)
polydf<-rowPerc(xtabs( ~polymer +effect, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame 
  filter(effect %in% c("Y","N"))%>% #Sorts into Yes and No
  rename(Type = "polymer")%>%#rename so future columns have same name 
  mutate(Type = case_when(
    Type == "BIO" ~ "Biopolymer",
    Type == "EVA" ~ "Polyethylene Vinyl Acetate",
    Type == "LTX" ~ "Latex",
    Type == "PA" ~ "Polyamide",
    Type == "PE" ~ "Polyethylene",
    Type == "PC" ~ "Polycarbonate",
    Type == "PET" ~ "Polyethylene Terephthalate",
    Type == "PI" ~ "Polyisoprene",
    Type == "PMMA" ~ "Polymethylmethacrylate",
    Type == "PP" ~ "Polypropylene",
    Type == "PS" ~ "Polystyrene",
    Type == "PUR" ~ "Polyurathane",
    Type == "PVC" ~ "Polyvinylchloride",
    Type == "PLA" ~ "Polyactic Acid"))%>%
  mutate_if(is.numeric, round,0)%>% #rounds percents 
  mutate(plot="Polymer") # change column name for check list
Endpoints<-xtabs(~polymer +effect ,aoc) #Pulls all study obs. for polymer from dataset
polyfinal<- data.frame(cbind(polyf, Endpoints))%>% #adds it as a column
  rename(Endpoints='Freq.1')%>% #renames column
  rename(category='polymer')#renames column

polyfinal



sizedf<-rowPerc(xtabs(~size.category +effect, aoc))
sizef<-as.data.frame(sizedf)%>%
  filter(effect %in% c("Y","N"))%>%
  mutate(size.category = case_when(
    size.category == 1 ~ "<1µm",
    size.category == 2 ~ "1µm < 10µm",
    size.category == 3 ~ "10µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "unavailable"))%>%
  rename(Type = "size.category")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Size")
study_s<-xtabs(~size.category +effect ,aoc)
sizefinal<- data.frame(cbind(sizef, study_s))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='size.category')

sizefinal

shapedf<-rowPerc(xtabs(~shape + effect, aoc))
shapef<-as.data.frame(shapedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")%>%
  mutate(Type = case_when(
    Type == "cube" ~ "Cube",
    Type == "sphere" ~ "Sphere",
    Type == "fragment" ~ "Fragment",
    Type == "fiber" ~ "Fiber"))
study_sh<-xtabs(~shape + effect,aoc)
shapefinal<- data.frame(cbind(shapef, study_sh))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='shape')
shapefinal


taxdf<-rowPerc(xtabs(~organism.group +effect, aoc))
taxf<-as.data.frame(taxdf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "organism.group")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Organism")
study_t<-xtabs(~organism.group +effect,aoc)
taxfinal<- data.frame(cbind(taxf, study_t))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='organism.group')

taxfinal


lvl1df<-rowPerc(xtabs(~lvl1 +effect, aoc))
lvl1f<-as.data.frame(lvl1df)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "lvl1")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Lvl1")%>%
  mutate(Type = case_when(
    Type == "alimentary.excretory" ~ "Alimentary, Excretory",
    Type == "behavioral.sense.neuro" ~ "Behavioral, Sensory, Neurological",
    Type == "circulatory.respiratory" ~ "Circulatory, Respiratory",
    Type == "community" ~ "Community",
    Type == "fitness" ~ "Fitness",
    Type == "immune" ~ "Immune",
    Type == "metabolism" ~ "Metabolism",
    Type == "microbiome" ~ "Microbiome",
    Type == "stress" ~ "Stress")) 
study_l<-xtabs(~lvl1 +effect,aoc)
lvl1final<- data.frame(cbind(lvl1f, study_l))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='lvl1')

lvl1final

lifedf<-rowPerc(xtabs(~life.stage +effect, aoc))
lifef<-as.data.frame(lifedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "life.stage")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Life.stage")
studyli<-xtabs(~life.stage +effect ,aoc)
lifefinal<- data.frame(cbind(lifef, studyli))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='life.stage')

lifefinal


vivodf<-rowPerc(xtabs(~invitro.invivo +effect, aoc))
vivof<-as.data.frame(vivodf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")%>%
  mutate(Type = case_when(
    Type=="invivo"~"In Vivo",
    Type=="invitro"~"In Vitro"))
study_v<-xtabs(~invitro.invivo +effect,aoc)
vivofinal<- data.frame(cbind(vivof, study_v))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='invitro.invivo')
vivofinal


routedf<-rowPerc(xtabs(~exposure.route +effect, aoc))
routef<-as.data.frame(routedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "exposure.route")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.route")%>%
  mutate(Type = case_when(
    Type == "coparental.exposure" ~"Co-Parental Exposure",
    Type == "paternal.exposure" ~ "Paternal Exposure",
    Type == "maternal.exposure" ~ "Maternal Exposure",
    Type == "food" ~ "Food",
    Type == "water" ~ "Water",
    Type == "sediment" ~ "Sediment"))
study_r<-xtabs(~exposure.route +effect,aoc)
routefinal<- data.frame(cbind(routef, study_r))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='exposure.route')
lifefinal
routefinal
polyfinal
sizefinal

A<-rbind(polyfinal,sizefinal)
B<-rbind(A,shapefinal)
C<-rbind(B,taxfinal)
D<-rbind(C,lvl1final)
E<-rbind(D,lifefinal)
G<-rbind(E,vivofinal)
Final_effect_dataset<-rbind(G,routefinal)

Final_effect_dataset2.0<-Final_effect_dataset%>%
  mutate(plot_f=factor(plot))

Final_effect_dataset2.0

write.csv(Final_effect_dataset2.0, "Final_effect_dataset.csv")
