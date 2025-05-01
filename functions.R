##### FUNCTIONS #####

#### Particle Characteristics Equations ####
############### SURFACE AREA ##################
#surface area equation for elongated spheres
SAfnx = function(length,
                 width = NA, 
                 height = NA,
                 R = NA,
                 H_W_ratio = 0.67# assumed 0.67 * width per Kooi et al. (2021)
) {
  # If width unknown, use L:W ratio
  width <- ifelse(is.na(width), R * length, width)
  
  # If height unknown, use H:R ratio
  height <- ifelse(is.na(height), H_W_ratio * width, height)
  
  # a, b, and c are equivalent to 1/2th of the length, width, and height, respectively
  a <- 0.5 * length
  b <- 0.5 * width
  c <- 0.5 * height
  
  SA = (4 * pi) * ((((a*b)^1.6 + (a*c)^1.6 + (b*c)^1.6) / 3) ^ (1/1.6))
  return(SA)}

### volume function
volumefnx <- function(R = NA, # average length-to-width ratio for environment
                      H_W_ratio = 0.67, # assumed 0.67 * width per Kooi et al. (2021)
                      length, # particle length (always known)
                      height = NA, # particle height (if known)
                      width = NA # particle width (if known)
) {
  # If width unknown, use L:W ratio
  width <- ifelse(is.na(width), R * length, width)
  
  # If height unknown, use H:R ratio
  height <- ifelse(is.na(height), H_W_ratio * width, height)
  
  # Calculate volume
  volume <- (4 / 3) * pi * (length / 2) * (width / 2) * (height / 2)
  
  return(volume)
}


################# MASS ####################
massfnx = function(v, p){
  mass = p * #density (g/cm^3)
    v * 1/1e12 * 1e6 #correction factor (g to ug)
  return(mass)}

#### Ecologically Relevant Metric Functions (used in reactives with user-input params) ####

###function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}



### Generalizable function that works on any value (alpha == 1 and == 2 are limits!)
mux_polyfnx <- function(a.x, x_UL, x_LL) {
  # Validate inputs
  if (length(a.x) != length(x_UL) || length(a.x) != length(x_LL)) {
    stop("a.x, x_UL, and x_LL must have the same length.")
  }
  
  # Initialize result vector
  mux.poly <- numeric(length(a.x))
  
  # Loop through each element to handle row-by-row logic
  for (i in seq_along(a.x)) {
    if (is.na(a.x[i]) || is.na(x_UL[i]) || is.na(x_LL[i])) {
      # Handle NA values
      mux.poly[i] <- NA
    } else if (a.x[i] == 1) {
      # Special case: a.x == 1
      if (x_UL[i] > 0 && x_LL[i] > 0) {
        mux.poly[i] <- (x_UL[i] - x_LL[i]) / log(x_UL[i] / x_LL[i])
      } else {
        mux.poly[i] <- NA  # Invalid input for log
      }
    } else if (a.x[i] == 2) {
      # Special case: a.x == 2
      epsilon <- 1e-10  # Small value to avoid division by zero
      if (x_UL[i] > 0 && x_LL[i] > 0) {
        mux.poly[i] <- log(x_UL[i] / x_LL[i]) /
          ((x_LL[i] + epsilon)^-1 - (x_UL[i] + epsilon)^-1)
      } else {
        mux.poly[i] <- NA  # Invalid input for log
      }
    } else {
      # General case: a.x != 1 and a.x != 2
      if (x_UL[i] > 0 && x_LL[i] > 0) {
        mux.poly[i] <- ((1 - a.x[i]) / (2 - a.x[i])) *
          ((x_UL[i]^(2 - a.x[i]) - x_LL[i]^(2 - a.x[i])) /
             (x_UL[i]^(1 - a.x[i]) - x_LL[i]^(1 - a.x[i])))
      } else {
        mux.poly[i] <- NA  # Invalid input for power calculations
      }
    }
  }
  
  # Return the result
  return(mux.poly)
}

#max ingestible specific surface area
SSA.inversefnx = function(sa, #surface area, calcaulted elsewhere
                          m){ #mass, calculated elsewhere
  SSA.inverse = m / sa
  return(SSA.inverse)}

#data tidying functions from Ana

############## Levels summary ##################
summarize_and_print <- function(data, column_name)
{
  result <- data %>%
    group_by({{ column_name }}) %>%
    summarise(n_datapoints = n()) %>%
    arrange(as.numeric(as.character({{ column_name }}))) %>%
    print(n = 1000)
  return(result)
}


############## Change particle length ##################
update_particle_length <- function(data, doi, length, polymer, shape, new_value) {
  data$Particle.Length..μm.[
    data$DOI == doi &
      data$Particle.Length..μm. == length &
      data$Polymer == polymer &
      data$Shape == shape
  ] = new_value
  
  return(data)
}


###### check what is missing #########
generate_structure_checks <- function(data) {
  structure.checks <- data.frame(
    na.counts = sapply(data, function(x) sum(is.na(x))),
    na.percent = round(sapply(data, function(x) sum(is.na(x)) / nrow(data) * 100), digits = 1),
    n.levels = sapply(data, function(x) length(unique(x)))
  )
  return(structure.checks)
  
}


#### CHECK PACKAGE ####
# Function to check and install a specific version of an R package
check_and_install_version <- function(package, version) {
  # Check if the package is installed
  if (requireNamespace(package, quietly = TRUE)) {
    # Get the installed version
    installed_version <- as.character(packageVersion(package))
    
    # Compare the installed version with the specified version
    if (installed_version == version) {
      message(paste("Package", package, "is already at the required version:", version))
    } else {
      message(paste("Package", package, "is installed but at version", installed_version, 
                    "instead of", version, ". Installing the specified version..."))
      remotes::install_version(package, version = version)
    }
  } else {
    # If the package is not installed, install the specified version
    message(paste("Package", package, "is not installed. Installing version", version, "..."))
    remotes::install_version(package, version = version)
  }
  
  # Verify the installation
  if (requireNamespace(package, quietly = TRUE)) {
    message(paste("Package", package, "is now at version:", as.character(packageVersion(package))))
  } else {
    stop(paste("Failed to install package", package, "version", version))
  }
}

# Example usage
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Check and install a specific version of ggplot2
#check_and_install_version("ggplot2", "3.3.5")

