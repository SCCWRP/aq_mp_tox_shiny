####### MASTER DATA TIDYING - MONTE CARLO  ########
###### 04-25-2024 #####
### This script runs the data preparations in a Monte Carlo fashion ##

set.seed(123456789) 

###### -------------- BASE DATA PREPARATION ------------------------- ###################
###### Define parameters used for particle property estimates ####
R.ave <- 0.77 # average length to width ratio of microplasticcs in marine environment (Kooi et al. 2021)
beta_log10_body_length <- 0.9341
body_length_intercept <- 1.1200

### --- LOAD FUNCTIONS --- ###
# each script is now a function. Source files to load functions #
source("monte carlo/RDAmaker_functions.R") #get ToMEx1.0 fxn
source("monte carlo/ToMEx2.0_Data_Tidying_functions.R") 

####### ---- RUN FUNCTIONS ---- ###
### Static with base parameters to ensure everything is working ###
## generate ToMEx 1.0 dataset ###
aoc_setup <- ToMEx1.0fxn(R.ave = R.ave,
                         beta_log10_body_length = beta_log10_body_length,
                         body_length_intercept = body_length_intercept) 

## generate ToMEx 2.0 dataset ###
tomex2.0_aoc_z_final <- ToMEx2.0fxn(aoc_setup = aoc_setup,
                                    R.ave = R.ave,
                                    beta_log10_body_length = beta_log10_body_length,
                                    body_length_intercept = body_length_intercept)

### Check if these files are identical to the legacy ones produced in RDAmaker.R and the data tidying script 
aoc_setup_OG <- read_rds("aoc_setup.RDS")
tomex2.0_aoc_z_final_OG <- read_rds("aoc_z_tomex2.RDS")

### these must return TRUE
identical(aoc_setup, aoc_setup_OG)
identical(tomex2.0_aoc_z_final %>% select(-rowid), #rowid is assigned randomly 
          tomex2.0_aoc_z_final_OG %>%  select(-rowid))

differences <- tomex2.0_aoc_z_final != tomex2.0_aoc_z_final_OG

# Assuming 'differences' is your logical matrix with TRUE/FALSE/NA values
# Set NAs to FALSE (if NA means no difference for your case)
differences[is.na(differences)] <- FALSE

# Get rows and columns indices where differences are TRUE
diff_indices <- which(differences, arr.ind = TRUE)

# Get unique rows and columns involved in differences
diff_rows <- unique(diff_indices[, "row"])
diff_cols <- unique(diff_indices[, "col"])

# If differences are found, print them out
if (length(diff_rows) > 0) {
  cat("Differences found in the following rows and columns:\n")
  for (i in seq_along(diff_rows)) {
    for (j in seq_along(diff_cols)) {
      if (differences[diff_rows[i], diff_cols[j]]) {  # Check if TRUE
        # Using column names instead of indices
        cat(sprintf("Difference at Row: %d, Column: %s\n",
                    diff_rows[i], colnames(differences)[diff_cols[j]]))
      }
    }
  }
} else {
  cat("No differences found.\n")
}

###### -------------- ALIGNMENTS ------------------------- ###################
### Define parameters
x1M_set <- 1 #um lower size for all alignments
x1D_set <- 1 #um lower size for all alignments
x2D_set <- 5000 #um
upper.tissue.trans.size.um <- 83 #10 #um #set size for x2M
alpha = 2.07 #table s4 for marine surface water. length
a.sa = 1.5 #marine surface area power law
a.v = 1.48 #a_V for marine surface water volume
a.m = 1.32 # upper limit fora_m for mass for marine surface water in table S4 
a.ssa = 1.98 # A_SSA for marine surface water
#define additional parameters for calculations based on averages in the environment
R.ave = 0.77 #average width to length ratio for microplastics in marine enviornment
p.ave = 1.10 #average density in marine surface water

### perform alignments
aoc_aligned_test <- tomex2.0_aoc_z_final  %>% 
  ungroup() %>% 
  rename(environment = env_f) %>% 
  ### First filter the data ####
## First filter data with global filters
  filter(!environment %in% c("Terrestrial", "Not Reported"),
         Group != "Bacterium",
         Group != "Plant",
         effect.metric != "HONEC",
         tier_zero_tech_f == "Red Criteria Passed",
         tier_zero_risk_f == "Red Criteria Passed", #All thresholds must pass technical and risk red criteria
         risk.13 != 0 #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been scored) - KEEP THIS AFTER THE RED CRITERIA FILTERS  
  ) %>% 
  #Remove 26C temperature treatment data from Jaimukar et al. 2018
  filter(!(article == 42 & media.temp == 26)) %>% 
  mutate(max.size.ingest.um = 1000 * max.size.ingest.mm) %>%  #makes it less confusing below
  # calculate ERM for each species
  #### TISSUE TRANSLOCATION ####
# define upper size length for Translocation 
#set to 83um for upper limit or max size ingest, whichever is smaller
mutate(x2M_trans = case_when(is.na(max.size.ingest.um) ~ upper.tissue.trans.size.um, 
                             max.size.ingest.um  < upper.tissue.trans.size.um ~  max.size.ingest.um,
                             max.size.ingest.um  > upper.tissue.trans.size.um ~ upper.tissue.trans.size.um)) %>% 
  
  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL_trans = dose.particles.mL.master) %>% 
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(mu.p.poly_trans = mux.polyfnx(a.x = alpha, #alpha for particles
                                       x_UL= x2M_trans, #upper ingestible size limit (width of particle)
                                       x_LL = x1M_set)) %>% 
  # polydisperse effect threshold for particles
  mutate(EC_poly_p.particles.mL_trans = (EC_mono_p.particles.mL_trans * mu.p.mono)/mu.p.poly_trans) %>% 
  #calculate CF_bio for all conversions
  mutate(CF_bio_trans = CFfnx(x1M = x1M_set,#lower size bin
                              x2M = x2M_trans, #upper translocatable
                              x1D = x1D_set, #default
                              x2D = x2D_set,  #default
                              a = alpha)) %>%  
  ## Calculate environmentally relevant effect threshold for particles
  mutate(EC_env_p.particles.mL_trans = EC_poly_p.particles.mL_trans * CF_bio_trans) %>%  #aligned particle effect concentraiton (1-5000 um)
  
  #### Surface area ERM ####
##--- environmental calculations ---###
#calculate lower translocatable surface area
mutate(x_LL_sa_trans = SAfnx(a = 0.5 * x1D_set, #length
                             b = 0.5 * x1D_set, #0.5 * R.ave * x1D_set, #width
                             c = 0.5 * x1D_set  #0.5 * R.ave * 0.67 * x1D_set #height
)) %>%  
  #calculate upper translocatable surface area
  mutate(x_UL_sa_trans = SAfnx(a = 0.5 * x2M_trans, 
                               b = 0.5 * x2M_trans, #width #0.5 * R.ave * x2M, 
                               c = 0.5 * x2M_trans #heigth #0.5 * R.ave * 0.67 * x2M
  )) %>%  
  #calculate mu_x_poly (env) for surface area
  mutate(mu.sa.poly_trans = mux.polyfnx(a.sa, x_UL_sa_trans, x_LL_sa_trans)) %>% 
  
  ##--- laboratory calculations ---###
  ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
  #(note that if mixed particles were used, a different equation must be used)
  mutate(mu.sa.mono = case_when(
    polydispersity == "monodisperse" ~ particle.surface.area.um2, # use reported surface area in monodisperse
    polydispersity == "polydisperse" ~  mux.polyfnx(a.x = a.sa, 
                                                    x_LL = particle.surface.area.um2.min,
                                                    x_UL = particle.surface.area.um2.max))) %>% 
  
  #calculate polydisperse effect concentration for surface area (particles/mL)
  mutate(EC_poly_sa.particles.mL_trans = (EC_mono_p.particles.mL_trans * mu.sa.mono)/mu.sa.poly_trans) %>%  
  #calculate environmentally realistic effect threshold
  mutate(EC_env_sa.particles.mL_trans = EC_poly_sa.particles.mL_trans * CF_bio_trans) %>% 
  
  ##### FOOD DILUTION ####
# define upper size length for ingestion 
mutate(x2M_ingest = case_when(is.na(max.size.ingest.um) ~ x2D_set, 
                              max.size.ingest.um < x2D_set ~ max.size.ingest.um,
                              max.size.ingest.um > x2D_set ~ x2D_set
)) %>%  #set to 5,000 as upper limit or max size ingest, whichever is smaller
  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL_ingest = dose.particles.mL.master) %>% 
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(mu.p.poly_ingest = mux.polyfnx(a.x = alpha, #alpha for particles
                                        x_UL= x2M_ingest, #upper ingestible size limit
                                        x_LL = x1M_set)) %>% 
  # polydisperse effect threshold for particles
  mutate(EC_poly_p.particles.mL_ingest = (EC_mono_p.particles.mL_ingest * mu.p.mono)/mu.p.poly_ingest) %>% 
  #calculate CF_bio for all conversions
  mutate(CF_bio_ingest = CFfnx(x1M = x1M_set,#lower size bin
                               x2M = x2M_ingest, #upper ingestible length
                               x1D = x1D_set, #default
                               x2D = x2D_set,  #default upper size range
                               a = alpha)) %>%  
  ## Calculate environmentally relevant effect threshold for particles
  mutate(EC_env_p.particles.mL_ingest = EC_poly_p.particles.mL_ingest * CF_bio_ingest) %>%  #aligned particle effect concentraiton (1-5000 um)
  
  
  #### volume ERM ####
##--- environmental calculations ---###
#calculate lower ingestible volume 
mutate(x_LL_v_ingest = volumefnx_poly(length = x1D_set,
                                      width = x1D_set)) %>% 
  #calculate maximum ingestible volume 
  mutate(x_UL_v_ingest = volumefnx_poly(length = x2M_ingest, # length-limited
                                        #x2D_set, #upper definiton (accouunts for fibers) CONSERVATIVE
                                        width = x2M_ingest)) %>% #ingestion-limited
  # calculate mu.v.poly
  mutate(mu.v.poly_ingest = mux.polyfnx(a.v, x_UL_v_ingest, x_LL_v_ingest)) %>% 
  ##--- laboratory calculations ---###
  ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
  #(note that if mixed particles were used, a different equation must be used)
  mutate(mu.v.mono = case_when(
    polydispersity == "monodisperse" ~ particle.volume.um3, # use reported volume in monodisperse
    polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.v, 
                                                   x_LL = particle.volume.um3.min,
                                                   x_UL = particle.volume.um3.max))) %>% 
  
  #calculate polydisperse effect concentration for volume (particles/mL)
  mutate(EC_poly_v.particles.mL_ingest = (EC_mono_p.particles.mL_ingest * mu.v.mono)/mu.v.poly_ingest) %>%  
  #calculate environmentally realistic effect threshold
  mutate(EC_env_v.particles.mL_ingest = EC_poly_v.particles.mL_ingest * CF_bio_ingest) %>% 
  
  ###### CLEANUP #####
mutate(particles.mL.ox.stress = EC_env_sa.particles.mL_trans,
       particles.mL.food.dilution = EC_env_v.particles.mL_ingest) %>% 
  select(-rowid) %>% 
  mutate(unique_id = row_number()) %>% 
 # mutate(unique_id = digest::digest(paste(across(everything()), collapse = "-"), algo = "md5")) %>%
  ungroup()

#### CHECKS TO MAKE SURE IT WORKS AS PLANNED ####
# load aligned dataset from ToMEx2.0_EcoToxRisk project (data/output/aoc_final.RDS)
aoc_aligned_ref <-read_rds("monte carlo/ref data/aoc_final.RDS") %>% select(-c(rowid, alpha, a.sa, a.v, a.m, a.ssa, R.ave, p.ave))

identical(aoc_aligned_test, 
          aoc_aligned_ref)
### Cool! ##



####### MONTE CARLO TIME, BABY!!! ####
##### STEP 1: DERIVE VALUES TO RUN PROBABILISTICALLY #####
n_sim <- 3

# particle properties
R.ave = 0.77 #average width to length ratio for microplastics in marine enviornment
R.ave.sd = 0.29 #Tablse s3. Marine surfac water
R.ave_samples <- rnorm(n_sim, mean = R.ave, sd = R.ave.sd)

p.ave = 1.10 #average density in marine surface water
p.ave.sd = 0.14 #Tablse s3. Marine surfac water
p.ave_samples <- rnorm(n_sim, mean = p.ave, sd = p.ave.sd)

# Alignment properties
alpha = 2.07 #table s4 for marine surface water. length
alpha.sd = 0.07 #table s4 for marine surface water. lengthj
alpha_samples <- rnorm(n_sim, mean = alpha, sd = alpha.sd)

# define parameters for power law coefficients
a.sa = 1.5 #marine surface area power law
a.sa.sd = 0.009 #marine surface water surface area power law - table s4
a.sa_samples <- rnorm(n_sim, mean = a.sa, sd = a.sa.sd)

a.v = 1.48 #a_V for marine surface water volume
a.v.sd = 0.063
a.v_samples <- rnorm(n_sim, mean = a.v, sd = a.v.sd)

a.m = 1.32 # upper limit fora_m for mass for marine surface water in table S4 
a.m.sd = 0.009
a.m_samples <- rnorm(n_sim, mean = a.m, sd = a.m.sd)

a.ssa = 1.98 # A_SSA for marine surface water
a.ssa.sd = 0.297
a.ssa_samples <- rnorm(n_sim, mean = a.ssa, sd = a.ssa.sd)

# Coefficients
beta_log10_body_length <- 0.9341
body_length_intercept <- 1.1200
# Standard errors for these coefficients
se_beta_log10_body_length <- 0.01  # Hypothetical value, replace with actual SE
se_body_length_intercept <- 0.02               # Hypothetical value, replace with actual SE
#data
sim_beta_log10_body_length_samples <- rnorm(n_sim, mean = beta_log10_body_length, sd = se_beta_log10_body_length)
sim_body_length_intercept_samples <- rnorm(n_sim, mean = body_length_intercept, sd = se_body_length_intercept)

MC_results <- vector("list", n_sim)

### MONTE CARLO LOOP ##
for (i in 1:n_sim) {
  #parameters to iterate
  alpha <- alpha_samples[i]
  a.sa <- a.sa_samples[i]
  a.v <- a.v_samples[i]
  a.m <- a.m_samples[i]
  a.ssa <- a.ssa_samples[i]
 # R.ave <- R.ave_samples[i] #length to width ratio (environment-specific)
  # p.ave <- p.ave_samples[i] #not used
  sim_beta_log10_body_length <- sim_beta_log10_body_length_samples[i]
  sim_body_length_intercept <- sim_body_length_intercept_samples[i]



####### ---- RUN FUNCTIONS ---- ###
### Static with base parameters to ensure everything is working ###
## generate ToMEx 1.0 dataset ###
aoc_setup <- ToMEx1.0fxn(R.ave = R.ave,
                         beta_log10_body_length = beta_log10_body_length,
                         body_length_intercept = body_length_intercept) 

## generate ToMEx 2.0 dataset ###
tomex2.0_aoc_z_final <- ToMEx2.0fxn(aoc_setup = aoc_setup,
                                    R.ave = R.ave,
                                    beta_log10_body_length = beta_log10_body_length,
                                    body_length_intercept = body_length_intercept)

### perform alignments
aoc_MC_iter <- tomex2.0_aoc_z_final  %>% 
  ungroup() %>% 
  rename(environment = env_f) %>% 
  ### First filter the data ####
## First filter data with global filters
filter(!environment %in% c("Terrestrial", "Not Reported"),
       Group != "Bacterium",
       Group != "Plant",
       effect.metric != "HONEC",
       tier_zero_tech_f == "Red Criteria Passed",
       tier_zero_risk_f == "Red Criteria Passed", #All thresholds must pass technical and risk red criteria
       risk.13 != 0 #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been scored) - KEEP THIS AFTER THE RED CRITERIA FILTERS  
) %>% 
  #Remove 26C temperature treatment data from Jaimukar et al. 2018
  filter(!(article == 42 & media.temp == 26)) %>% 
  mutate(max.size.ingest.um = 1000 * max.size.ingest.mm) %>%  #makes it less confusing below
  # calculate ERM for each species
  #### TISSUE TRANSLOCATION ####
# define upper size length for Translocation 
#set to 83um for upper limit or max size ingest, whichever is smaller
mutate(x2M_trans = case_when(is.na(max.size.ingest.um) ~ upper.tissue.trans.size.um, 
                             max.size.ingest.um  < upper.tissue.trans.size.um ~  max.size.ingest.um,
                             max.size.ingest.um  > upper.tissue.trans.size.um ~ upper.tissue.trans.size.um)) %>% 
  
  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL_trans = dose.particles.mL.master) %>% 
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(mu.p.poly_trans = mux.polyfnx(a.x = alpha, #alpha for particles
                                       x_UL= x2M_trans, #upper ingestible size limit (width of particle)
                                       x_LL = x1M_set)) %>% 
  # polydisperse effect threshold for particles
  mutate(EC_poly_p.particles.mL_trans = (EC_mono_p.particles.mL_trans * mu.p.mono)/mu.p.poly_trans) %>% 
  #calculate CF_bio for all conversions
  mutate(CF_bio_trans = CFfnx(x1M = x1M_set,#lower size bin
                              x2M = x2M_trans, #upper translocatable
                              x1D = x1D_set, #default
                              x2D = x2D_set,  #default
                              a = alpha)) %>%  
  ## Calculate environmentally relevant effect threshold for particles
  mutate(EC_env_p.particles.mL_trans = EC_poly_p.particles.mL_trans * CF_bio_trans) %>%  #aligned particle effect concentraiton (1-5000 um)
  
  #### Surface area ERM ####
##--- environmental calculations ---###
#calculate lower translocatable surface area
mutate(x_LL_sa_trans = SAfnx(a = 0.5 * x1D_set, #length
                             b = 0.5 * x1D_set, #0.5 * R.ave * x1D_set, #width
                             c = 0.5 * x1D_set  #0.5 * R.ave * 0.67 * x1D_set #height
)) %>%  
  #calculate upper translocatable surface area
  mutate(x_UL_sa_trans = SAfnx(a = 0.5 * x2M_trans, 
                               b = 0.5 * x2M_trans, #width #0.5 * R.ave * x2M, 
                               c = 0.5 * x2M_trans #heigth #0.5 * R.ave * 0.67 * x2M
  )) %>%  
  #calculate mu_x_poly (env) for surface area
  mutate(mu.sa.poly_trans = mux.polyfnx(a.sa, x_UL_sa_trans, x_LL_sa_trans)) %>% 
  
  ##--- laboratory calculations ---###
  ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
  #(note that if mixed particles were used, a different equation must be used)
  mutate(mu.sa.mono = case_when(
    polydispersity == "monodisperse" ~ particle.surface.area.um2, # use reported surface area in monodisperse
    polydispersity == "polydisperse" ~  mux.polyfnx(a.x = a.sa, 
                                                    x_LL = particle.surface.area.um2.min,
                                                    x_UL = particle.surface.area.um2.max))) %>% 
  
  #calculate polydisperse effect concentration for surface area (particles/mL)
  mutate(EC_poly_sa.particles.mL_trans = (EC_mono_p.particles.mL_trans * mu.sa.mono)/mu.sa.poly_trans) %>%  
  #calculate environmentally realistic effect threshold
  mutate(EC_env_sa.particles.mL_trans = EC_poly_sa.particles.mL_trans * CF_bio_trans) %>% 
  
  ##### FOOD DILUTION ####
# define upper size length for ingestion 
mutate(x2M_ingest = case_when(is.na(max.size.ingest.um) ~ x2D_set, 
                              max.size.ingest.um < x2D_set ~ max.size.ingest.um,
                              max.size.ingest.um > x2D_set ~ x2D_set
)) %>%  #set to 5,000 as upper limit or max size ingest, whichever is smaller
  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL_ingest = dose.particles.mL.master) %>% 
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(mu.p.poly_ingest = mux.polyfnx(a.x = alpha, #alpha for particles
                                        x_UL= x2M_ingest, #upper ingestible size limit
                                        x_LL = x1M_set)) %>% 
  # polydisperse effect threshold for particles
  mutate(EC_poly_p.particles.mL_ingest = (EC_mono_p.particles.mL_ingest * mu.p.mono)/mu.p.poly_ingest) %>% 
  #calculate CF_bio for all conversions
  mutate(CF_bio_ingest = CFfnx(x1M = x1M_set,#lower size bin
                               x2M = x2M_ingest, #upper ingestible length
                               x1D = x1D_set, #default
                               x2D = x2D_set,  #default upper size range
                               a = alpha)) %>%  
  ## Calculate environmentally relevant effect threshold for particles
  mutate(EC_env_p.particles.mL_ingest = EC_poly_p.particles.mL_ingest * CF_bio_ingest) %>%  #aligned particle effect concentraiton (1-5000 um)
  
  
  #### volume ERM ####
##--- environmental calculations ---###
#calculate lower ingestible volume 
mutate(x_LL_v_ingest = volumefnx_poly(length = x1D_set,
                                      width = x1D_set)) %>% 
  #calculate maximum ingestible volume 
  mutate(x_UL_v_ingest = volumefnx_poly(length = x2M_ingest, # length-limited
                                        #x2D_set, #upper definiton (accouunts for fibers) CONSERVATIVE
                                        width = x2M_ingest)) %>% #ingestion-limited
  # calculate mu.v.poly
  mutate(mu.v.poly_ingest = mux.polyfnx(a.v, x_UL_v_ingest, x_LL_v_ingest)) %>% 
  ##--- laboratory calculations ---###
  ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
  #(note that if mixed particles were used, a different equation must be used)
  mutate(mu.v.mono = case_when(
    polydispersity == "monodisperse" ~ particle.volume.um3, # use reported volume in monodisperse
    polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.v, 
                                                   x_LL = particle.volume.um3.min,
                                                   x_UL = particle.volume.um3.max))) %>% 
  
  #calculate polydisperse effect concentration for volume (particles/mL)
  mutate(EC_poly_v.particles.mL_ingest = (EC_mono_p.particles.mL_ingest * mu.v.mono)/mu.v.poly_ingest) %>%  
  #calculate environmentally realistic effect threshold
  mutate(EC_env_v.particles.mL_ingest = EC_poly_v.particles.mL_ingest * CF_bio_ingest) %>% 
  
  ###### CLEANUP #####
mutate(particles.mL.ox.stress = EC_env_sa.particles.mL_trans,
       particles.mL.food.dilution = EC_env_v.particles.mL_ingest) %>% 
  #rowwise() %>%
  mutate(unique_id = row_number()) %>% 
 # mutate(unique_id = digest::digest(paste(across(everything()), collapse = "-"), algo = "md5")) %>%
  ungroup()

MC_results[[i]] <- list(particles_mL_ox_stress = aoc_MC_iter$particles.mL.ox.stress,
                        particles_mL_food_dilution = aoc_MC_iter$particles.mL.food.dilution,
                        unique_id = aoc_MC_iter$unique_id)
}


##### Determine Coefficient of Variation for each output ###
### Step 1: Create a dataframe from the list
results_df <- do.call(rbind, lapply(MC_results, function(x) {
  data.frame(
    unique_id = x$unique_id,
    particles_L_ox_stress = x$particles_mL_ox_stress *1000,
    particles_L_food_dilution = x$particles_mL_food_dilution * 1000
  )
}))
skimr::skim(results_df)

## Step 2:
# Calculate CoV
cov_results <- results_df %>%
  group_by(unique_id) %>%
  summarise(sd_particles_L_ox_stress = sd(particles_L_ox_stress, na.rm = TRUE),
            mean_particles_L_ox_stress = mean(particles_L_ox_stress, na.rm = TRUE),
            median_particles_L_ox_stress = median(particles_L_ox_stress, na.rm = TRUE),
            CoV_ox_stress = sd_particles_L_ox_stress / mean_particles_L_ox_stress,
            #food dilution
            sd_particles_L_food_dilution = sd(particles_L_food_dilution, na.rm = TRUE),
            mean_particles_L_food_dilution = mean(particles_L_food_dilution, na.rm = TRUE),
            median_particles_L_food_dilution = median(particles_L_food_dilution, na.rm = TRUE),
            CoV_food_dilution = sd_particles_L_food_dilution / mean_particles_L_food_dilution
  )

# View the results
skimr::skim(cov_results)

# Step 3: merge with full dataset and deterministic dataset
aoc_MC <- merge(aoc_aligned_test, cov_results, by = "unique_id", all.x = TRUE)
aoc_MC <- left_join(aoc_MC, 
                    aoc_aligned_test %>% ungroup() %>%  
                      select(c(unique_id, particles.mL.food.dilution, particles.mL.ox.stress)) %>%
                      mutate(particles_L_food_dilution_deterministic = particles.mL.food.dilution * 1000,
                             particles_L_ox_stress_deterministic = particles.mL.ox.stress * 1000),
                    by = "unique_id"
) #%>% 
#filter(size.length.um.used.for.conversions >= x1M_set) %>% #alignments not valid below 1 um, so filter them before they become problems later

# summary stats to ensure MC was succesful 
aoc_MC %>% 
  select(c(particles_L_ox_stress_deterministic, median_particles_L_ox_stress,
           particles_L_food_dilution_deterministic, median_particles_L_food_dilution)) %>% 
  mutate(ox_stress_ratio = particles_L_ox_stress_deterministic / median_particles_L_ox_stress,
         food_dilution_ratio = particles_L_food_dilution_deterministic / median_particles_L_food_dilution)

#define limits
min_val <- min(aoc_MC$CoV_food_dilution, na.rm = TRUE)
max_val <- max(aoc_MC$CoV_food_dilution, na.rm = TRUE)

# You may want to add some padding around the min and max
pad <- (max_val - min_val) * 0.05
xlims <- c(min_val - pad, max_val + pad)

hist_cv_food <- aoc_MC %>% 
  ggplot(aes(x = CoV_food_dilution, fill = polydispersity)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  xlim(xlims) +  # Set x limits
  scale_x_log10() +
  xlab("Coefficient of Variation (unitless)") +
  ylab("Toxicity Data Points")# +
  #fill.type +
#  theme.light #+
#theme(axis.title.x = element_blank())

hist_cv_ox <- aoc_MC %>% 
  ggplot(aes(x = CoV_ox_stress, fill = polydispersity)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  #fill.type +
  scale_x_log10() +
  # xlim(xlims) +  # Set x limits
  xlab("Coefficient of Variation (unitless)") +
  ylab("Toxicity Data Points") +
 # theme.light +
  theme(legend.position = "none",
        axis.title.y = element_blank())

CoV_plots <-  ggpubr::ggarrange(hist_cv_food, hist_cv_ox,
                       labels = c("A", "B"),
                       ncol = 2,
                       common.legend = TRUE, legend = "top")

CoV_plots
