
aoc_explore <- aoc %>% 
  
  #Factors
  mutate(effect_f = as.factor(effect)) %>% 
  
  mutate(size_f = as.factor(size.category)) %>% 
  
  mutate(shape_f = as.factor(shape)) %>% 
  
  mutate(poly_f = as.factor(polymer)) %>% 
  
  mutate(org_f = as.factor(organism.group)) %>% 
  
  mutate(lvl1_f = as.factor(lvl1)) %>% 
  
  mutate(lvl2_f = as.factor(lvl2)) %>% 
  
  mutate(lvl3_f = as.factor(lvl3)) %>% 
  
  mutate(bio_f = as.factor(bio.org)) %>% 
  
  mutate(vivo_f = as.factor(invitro.invivo)) %>% 
           
  mutate(life_f = as.factor(life.stage)) %>% 
    
  mutate(env_f = as.factor(environment)) %>% 
    
  mutate(weather.biofoul_f = as.factor(weather.biofoul)) %>% 
    
  mutate(species_f = as.factor(paste(genus,species))) %>% 
    
  mutate(dose.mg.L.master.converted.reported = factor(dose.mg.L.master.converted.reported)) %>%
  
  mutate(dose.particles.mL.master.converted.reported = factor(dose.particles.mL.master.converted.reported)) %>% 
  
  mutate(effect.metric = factor(effect.metric)) %>% 
    
  mutate(af.time_noNA = replace_na(af.time, "Unavailable")) %>%
    
  mutate(acute.chronic_f = factor(acute.chronic)) %>%
    
  mutate(tier_zero_tech_f = factor(tech.tier.zero)) %>%

  mutate(tier_zero_risk_f = factor(risk.tier.zero)) %>%
  
  mutate(exp_type_f = factor(experiment.type)) %>% 
  
  mutate(max.size.ingest.mm = ifelse(is.na(max.size.ingest.mm), 
                                     10^(0.9341 * log10(body.length.cm) - 1.1200) * 10,  #(Jamm et al 2020 Nature paper)correction for cm to mm
                                     max.size.ingest.mm)) %>%  # if already present, just use that
  mutate(max.size.ingest.um = 1000 * max.size.ingest.mm) %>%  #makes it less confusing below
  #calcualte monodisperse unaligned effect concentration for surface area
  mutate(dose.um2.mL.master = as.numeric(particle.surface.area.um2) * dose.particles.mL.master) %>% 
  #calculate monodisperse unaligned  specific surface area dose
  mutate(dose.um2.ug.mL.master = dose.um2.mL.master / (mass.per.particle.mg / 1000)) #correct mg to ug
  
 
    
  
    
  
    
    
    