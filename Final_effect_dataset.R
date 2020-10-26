aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)
polydf<-rowPerc(xtabs( ~polymer +effect, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame 
  filter(effect %in% c("Y","N"))%>% #Sorts into Yes and No
  rename(type= "polymer")%>% #rename so future columns have same name 
  mutate_if(is.numeric, round,0)%>% #rounds percents 
  mutate(plot="Polymer") # change column name for check list
study<-xtabs(~polymer,aoc) #Pulls all study obs. for polymer from dataset
polyfinal<- data.frame(cbind(polyf, study))%>% #adds it as a column
  rename(study='Freq.1')%>% #renames column
  rename(category='polymer')#renames column
polyfinal$study[duplicated(polyfinal$study)]<-NA #replaces duplicated values with NA


sizedf<-rowPerc(xtabs(~size.category +effect, aoc))
sizef<-as.data.frame(sizedf)%>%
  filter(effect %in% c("Y","N"))%>%
  mutate(size.category = case_when(
    size.category == 1 ~ "<1µ",
    size.category == 2 ~ "1µ < 10µ",
    size.category == 3 ~ "10µ < 100µ",
    size.category == 4 ~ "100µ < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "unavailable"))%>%
  rename(type= "size.category")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Size")
study_s<-xtabs(~size.category,aoc)
sizefinal<- data.frame(cbind(sizef, study_s))%>% 
  rename(study='Freq.1')%>%
  rename(category='size.category')
sizefinal$study[duplicated(sizefinal$study)]<-NA 

shapedf<-rowPerc(xtabs(~shape + effect, aoc))
shapef<-as.data.frame(shapedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")
study_sh<-xtabs(~shape,aoc)
shapefinal<- data.frame(cbind(shapef, study_sh))%>% 
  rename(study='Freq.1')%>%
  rename(category='shape')
shapefinal$study[duplicated(shapefinal$study)]<-NA

taxdf<-rowPerc(xtabs(~organism.group +effect, aoc))
taxf<-as.data.frame(taxdf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "organism.group")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Organism")
study_t<-xtabs(~organism.group,aoc)
taxfinal<- data.frame(cbind(taxf, study_t))%>% 
  rename(study='Freq.1')%>%
  rename(category='organism.group')
taxfinal$study[duplicated(taxfinal$study)]<-NA


lvl1df<-rowPerc(xtabs(~lvl1 +effect, aoc))
lvl1f<-as.data.frame(lvl1df)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "lvl1")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Lvl1")
study_l<-xtabs(~lvl1,aoc)
lvl1final<- data.frame(cbind(lvl1f, study_l))%>% 
  rename(study='Freq.1')%>%
  rename(category='lvl1')
lvl1final$study[duplicated(lvl1final$study)]<-NA


lifedf<-rowPerc(xtabs(~life.stage +effect, aoc))
lifef<-as.data.frame(lifedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "life.stage")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Life.stage")
studyli<-xtabs(~life.stage,aoc)
lifefinal<- data.frame(cbind(lifef, studyli))%>% 
  rename(study='Freq.1')%>%
  rename(category='life.stage')
lifefinal$study[duplicated(lifefinal$study)]<-NA



vivodf<-rowPerc(xtabs(~invitro.invivo +effect, aoc))
vivof<-as.data.frame(vivodf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")
study_v<-xtabs(~invitro.invivo,aoc)
vivofinal<- data.frame(cbind(vivof, study_v))%>% 
  rename(study='Freq.1')%>%
  rename(category='invitro.invivo')
vivofinal$study[duplicated(vivofinal$study)]<-NA


routedf<-rowPerc(xtabs(~exposure.route +effect, aoc))
routef<-as.data.frame(routedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "exposure.route")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.route")
study_r<-xtabs(~exposure.route,aoc)
routefinal<- data.frame(cbind(routef, study_r))%>% 
  rename(study='Freq.1')%>%
  rename(category='exposure.route')
routefinal$study[duplicated(routefinal$study)]<-NA

A<-rbind(polyfinal,sizefinal)
B<-rbind(A,shapefinal)
C<-rbind(B,taxfinal)
D<-rbind(C,lvl1final)
E<-rbind(D,lifefinal)
G<-rbind(E,vivofinal)
Final_effect_dataset<-rbind(G,routefinal)

Final_effect_dataset<-Final_effect_dataset%>%
  mutate(plot_f=factor(plot))

Final_effect_dataset

write_csv(Final_effect_dataset, 'Final_effect_dataset')
