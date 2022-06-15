# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(tidyverse)

###########################
# Higher irreplaceable areas within PAs
###########################
ir <- raster("outputs/irreplacecable_areas.tif")
pa <- raster::raster("data/layer/pa_bd_up.tif")

# Resample PA layer
pa <- resample(pa, ir, method = "ngb")

# Identifying top 10% values
data <- as.data.frame(table(getValues(ir)))
data <- data[2:52797,] # Removing 0
data1 <- data %>% slice_max(Var1, prop = 0.1)
ir_high <- ir >= 0.00834591686725616 # Total cells = 5303
writeRaster(ir_high, "outputs/ir_high.tif")

ir_high_within_pa <- cellStats(raster::Which(pa > 0 & ir_high > 0), "sum")
ir_high_within_pa/5303*100

# Priority areas with very low score
data2 <- data %>% slice_max(Var1, prop = 0.7)

###########################
# LC + Prioritization + Irreplaceable areas
###########################
lc <- raster("data/clim/lc_up.tif")
priori <- raster("outputs/spatial_prioritization.tif")
ir_high <- raster("outputs/ir_high.tif")

# Extracting land-use types
shrub <- lc == 20
herb <- lc == 30
crop <- lc == 40
water <- lc %in% c(80, 200)
herb_wetland <- lc == 90
forest <- lc %in% c(111:116, 121:126)

# Prioritization area by land class types
shrub_priori <- cellStats(raster::Which(shrub > 0 & priori > 0), "sum")
herb_priori <- cellStats(raster::Which(herb > 0 & priori > 0), "sum")
crop_priori <- cellStats(raster::Which(crop > 0 & priori > 0), "sum")
water_priori <- cellStats(raster::Which(water > 0 & priori > 0), "sum")
herb_wetland_priori <- cellStats(raster::Which(herb_wetland > 0 & priori > 0), "sum")
forest_priori <- cellStats(raster::Which(forest > 0 & priori > 0), "sum")

# Converting it into a dataframe
lc_priori <- as.data.frame(rbind(shrub_priori, herb_priori, crop_priori, water_priori,
                                 herb_wetland_priori, forest_priori))
colnames(lc_priori) <- c("ncell")

lc_priori <- lc_priori %>% 
  mutate(lc = rownames(lc_priori),
         per = ncell/sum(ncell)*100)

write_csv(lc_priori, "outputs/lc_priori.csv")
  
# High IR area by land class types
shrub_ir <- cellStats(raster::Which(shrub > 0 & ir_high > 0), "sum")
herb_ir <- cellStats(raster::Which(herb > 0 & ir_high > 0), "sum")
crop_ir <- cellStats(raster::Which(crop > 0 & ir_high > 0), "sum")
water_ir <- cellStats(raster::Which(water > 0 & ir_high > 0), "sum")
herb_wetland_ir <- cellStats(raster::Which(herb_wetland > 0 & ir_high > 0), "sum")
forest_ir <- cellStats(raster::Which(forest > 0 & ir_high > 0), "sum")

# Converting it into a dataframe
lc_ir <- as.data.frame(rbind(shrub_ir, herb_ir, crop_ir, water_ir,
                                 herb_wetland_ir, forest_ir))
colnames(lc_ir) <- c("ncell")

lc_ir <- lc_ir %>% 
  mutate(lc = rownames(lc_ir),
         per = ncell/sum(ncell)*100)

write_csv(lc_ir, "outputs/lc_ir.csv")

###########################
# HFP and prioritization + Irreplaceability scores
###########################
priori <- raster("outputs/spatial_prioritization.tif")
hfp <- raster("data/layer/hfp_bd_re.tif")

r <- raster::stack(priori, hfp)

vals <- values(r)
coord <- xyFromCell(r, 1:ncell(r))
combine <- cbind(coord, vals)
combine <- as.data.frame(combine)
head(combine)

write_csv(combine, "outputs/priori_hfp_data.csv")

###########################
# Divisions and prioritization
###########################
# Exporting clipped layers
dhk <- raster("data/layer/Dhaka_sp_up.tif")
kh <- raster("data/layer/Khulna_sp_up.tif")
cht <- raster("data/layer/Chittagong_sp_up.tif")
bar <- raster("data/layer/Barisal_sp_up.tif")
rang <- raster("data/layer/Rangpur_sp_up.tif")
raj <- raster("data/layer/Rajshahi_sp_up.tif")
syl <- raster("data/layer/Sylhet_sp_up.tif")

# Calcluating proportions of priority areas by division
dhk_sp <- as.data.frame(table(getValues(dhk)))
dhk_sp <- dhk_sp %>% 
  mutate(div = "Dhaka", prop = Freq/sum(Freq)*100)

kh_sp <- as.data.frame(table(getValues(kh)))
kh_sp <- kh_sp %>% 
  mutate(div = "Khulna", prop = Freq/sum(Freq)*100)

cht_sp <- as.data.frame(table(getValues(cht)))
cht_sp <- cht_sp %>% 
  mutate(div = "Chittagong", prop = Freq/sum(Freq)*100)

bar_sp <- as.data.frame(table(getValues(bar)))
bar_sp <- bar_sp %>% 
  mutate(div = "Barisal", prop = Freq/sum(Freq)*100)

rang_sp <- as.data.frame(table(getValues(rang)))
rang_sp <- rang_sp %>% 
  mutate(div = "Rangpur", prop = Freq/sum(Freq)*100)

raj_sp <- as.data.frame(table(getValues(raj)))
raj_sp <- raj_sp %>% 
  mutate(div = "Rajshahi", prop = Freq/sum(Freq)*100)

syl_sp <- as.data.frame(table(getValues(syl)))
syl_sp <- syl_sp %>% 
  mutate(div = "Sylhet", prop = Freq/sum(Freq)*100)

priori_division <- rbind(dhk_sp, kh_sp, cht_sp, bar_sp, rang_sp, raj_sp, syl_sp)

priori_division_pr <- priori_division %>% 
  filter(Var1 == 1) %>% 
  mutate(prop_total = Freq/55708*100) # Total pixels selected in the prioritization

write_csv(priori_division_pr, "outputs/priori_division_pr.csv")
