# Loading required libraries
library(slam)
library(gurobi)
library(raster)
library(sp)
library(prioritizr)
library(tidyverse)

# # Renaming files
# tifs <- list.files(path = "binaryRasters/", pattern = ".tif", recursive = TRUE, full.names = TRUE) 
# 
# sapply(tifs,FUN=function(eachPath){file.rename(from=eachPath,to=sub(pattern="Binary",replacement="",eachPath))
# })
# 

# prep feature data
features <- here::here("R:/BUTTERFL19-A1712/IMPORTANT/SideProjects/Bangladesh/NationalAssessment/ConservationPlanning/outputs/BinaryRasters_Built-upAreasRemoved/") %>%
  list.files(pattern = "*.tif$", full.names = TRUE) %>%
  raster::stack()

# create the pu file 
pu <- features[[1]]
pu[][!is.na(pu[])] <- 1

# create an rij matrix to speed up the prioritization process. 
# especially useful if you run several scenarios
# just create it once and store to disk
if(!file.exists("rij.rds")){
  rij <- rij_matrix(pu, features) * 0.693 # converting it to km2 to match other data
  rij %>% saveRDS("rij.rds", compress = FALSE) 
  
} else {
  
  rij <- readRDS("rij.rds") 
  
}

# features
spec <- tibble(id = 1:nlayers(features),
               name = names(features))

# Reading specie-wise target data file
full_data <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")

# Converting percentages to proportion and calculating target area
full_data <- full_data %>% 
  dplyr::mutate(prop = TargetCoverage/100, spp_target_area = prop*sdm.area)

# Selecting required columns
full_data <- full_data %>% 
  dplyr::select(species, spp_target_area)

# Renaming columns
colnames(full_data)[1] <- c("name")

# Joining with the feature file
spec <- dplyr::inner_join(spec, full_data, by = "name")

# Adding PA data
pa <- raster::raster("data/layer/pa_bd_up.tif")

# Resample PA layer
raster_file <- raster("outputs/BinaryRasters_Built-upAreasRemoved/Chloropsis_hardwickii.tif")
pa <- resample(pa, raster_file, method = "ngb")

pa <- ifelse(!is.na(pa[][!is.na(pu[])]), TRUE, FALSE)


# setup the problem
p1 <- problem(rep(1, ncol(rij)),
              features = spec,
              rij_matrix = rij,
              run_checks = FALSE) %>%
  add_min_set_objective() %>%
  add_absolute_targets("spp_target_area") %>%
  add_binary_decisions() %>% 
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores() - 4)

# Locked in PA
p1 <- p1 %>%
  add_locked_in_constraints(pa)

# with HF as cost

#p1 with HF penalty
hfp <- raster::raster("data/layer/hfp_bd_re.tif")

# # Resample hfp
# crs(hfp) <- crs(raster_file)
# extent(hfp) <- extent(raster_file)
# hfp <- resample(hfp, raster_file, method = "ngb")

hfp_val <- hfp[][!is.na(pu[])]
hfp_val <- ifelse(is.na(hfp_val), 0, hfp_val)

p2 <- p1 %>% 
  add_linear_penalties(1, hfp_val)

# solve the problem
s2 <- solve(p2, force = TRUE)

#################################
# Optimal solution found (tolerance 1.00e-01)
# Best objective 7.509689226403e+05, best bound 7.497562700475e+05, gap 0.1615%
# Best objective 1.301105220342e+06, best bound 1.300257587665e+06, gap 0.0651% [new]
#################################

# as we use rij matrix values to speed things up, we need to convert the results
# back to raster to plot them
r2 <- pu
r2_val <- r2[][!is.na(pu[])]
r2_val <- s2
r2[][!is.na(pu[])] <- r2_val

# save results
r2 %>% writeRaster("outputs/spatial_prioritization_up.tif", overwrite = TRUE)

# Details on critical conservation areas
table(getValues(r2)) # 0: 90243; 1: 83954
area <- 83954*0.693 # 58180.122 km2
prop <- 58180.122/148460*100 # 39.19%

##############################################################
# Irreplaceability
# calculate Ferrier scores and extract total score
# ir2 <- eval_ferrier_importance(p2, s2)[["total"]]
ir2 <- eval_ferrier_importance(p2, s2)

ir_total <- ir2[, "total"]

# as we use rij matrix values to speed things up, we need to convert the results
# back to raster to plot them
ir_raster <- pu
ir_raster[!is.na(pu)] <- ir_total

ir_raster %>% writeRaster("outputs/irreplacecable_areas_up2.tif", overwrite = TRUE)
