library(ggplot2)
library(tidyverse)

# Loading data file
df <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
head(df)

# Mean coverage by taxa and IUCN status [barplot]
taxa_iucn <- df %>%
  group_by(group, nationalStatus, threatStatus) %>%
  summarise(mean_coverage = mean(sdm))
head(taxa_iucn)

taxa_iucn1 <- taxa_iucn %>% 
  dplyr::filter(group %in% c("Amphibians", "Birds", "Crustaceans", "Freshwater fishes", "Mammals", "Reptiles"))

taxa_iucn1 %>% 
  dplyr::mutate(x = fct_reorder(nationalStatus, mean_coverage)) %>% 
  ggplot( aes(x, mean_coverage, fill = threatStatus)) +
  geom_bar(stat = "identity", position = "identity") +
  theme_classic() + facet_wrap(~group) + xlab("") + ylab("Mean PA coverage (%)") + 
  scale_fill_manual(values = c("cadetblue2","deepskyblue4", "darkgoldenrod1"))  +
  theme(legend.position = "none", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("outputs/Figures/meanPACoverage_excludingButterflies.png")

taxa_iucn2 <- taxa_iucn %>% 
  dplyr::filter(group %in% c("Butterflies"))

taxa_iucn2 %>% 
  ggplot( aes(nationalStatus, mean_coverage, fill = threatStatus)) +
  geom_bar(stat = "identity", position = "identity") +
  theme_classic() + facet_wrap(~group) + xlab("") + ylab("Mean PA coverage (%)") +
  theme(legend.position = "none", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("cadetblue2","deepskyblue4", "darkgoldenrod1"))

ggsave("outputs/Figures/meanPACoverage_Butterflies.png")

# Number of species in taxa and IUCN status combinations
taxa_iucn_n <- df %>%
  group_by(group, nationalStatus) %>%
  summarise(n = NROW(species))
head(taxa_iucn_n)

write_csv(taxa_iucn_n, "outputs/taxa_iucn_n.csv")

################
# Representation gap between the threatened and non-threatened species
################

# Loading data file
df <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
head(df)

df1 <- df %>% 
  dplyr::filter(!threatStatus == "Data-Deficient")

head(df1)

# Boxplot
ggplot(df1, aes(threatStatus, gap, fill = threatStatus, col = threatStatus)) +
  geom_boxplot() + theme_classic() +
  xlab("") + ylab("Representation gap (%)") +
  scale_fill_manual(values = c("deepskyblue4","darkgoldenrod1")) +
  facet_wrap(~group, nrow = 4) + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("deepskyblue4","darkgoldenrod1"))

# Histogram
ggplot(df1, aes(gap, fill = threatStatus, col = threatStatus)) +
  geom_histogram(bins = 100) + theme_classic() +
  xlab("Target shortfall (%)") + ylab("Number of species") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  facet_wrap(~group, nrow = 4) + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("deepskyblue4", "darkgoldenrod1")) + 
  geom_vline(aes(xintercept= 49.14), color="black", linetype="dashed", size=1)

ggsave("outputs/Figures/representationGap_threantened_non-threatened.png")

###################
# LC and prioritization
###################
lc_priori <- read_csv("outputs/lc_priori.csv")
head(lc_priori)

lc_priori %>% 
  dplyr::mutate(x = fct_reorder(lc, per)) %>% 
  ggplot( aes(x, per)) +
  geom_bar(stat = "identity", position = "identity", fill = "darkgoldenrod1") +
  theme_classic() + xlab("") + ylab("")  +
  theme(legend.position = "none", legend.title = element_blank())

ggsave("outputs/Figures/lc_priori.png")

###################
# HFP and Priori
###################
hfp_priori <- read.csv("outputs/priori_hfp_data.csv")
head(hfp_priori)

bin_size <- 5

hfp_priori %>% 
  filter(spatial_prioritization == 1) %>% 
  mutate(bin_y = factor(hfp_bd_re%/%bin_size)) %>% 
  ggplot(aes(bin_y, spatial_prioritization)) +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  theme_classic() + 
  labs(x = "", y = "")

ggsave("outputs/Figures/hfp_priori.png")

###################
# Threatened percentage
###################
data <- read.csv("data/test.csv")
head(data)

# Hole size
hsize <- 2

data <- data %>% 
  mutate(x = hsize)

# Amphibians
data %>% 
  filter(group == "Amphibians") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Amphibians_threat_percentages.png")

# Butterflies
data %>% 
  filter(group == "Butterflies") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Butterflies_threat_percentages.png")

# Mammals
data %>% 
  filter(group == "Mammals") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Mammals_threat_percentages.png")

# Freshwater fishes
data %>% 
  filter(group == "Freshwater fishes") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Freshwater fishes_threat_percentages.png")

# Reptiles
data %>% 
  filter(group == "Reptiles") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Reptiles_threat_percentages.png")

# Crustaceans
data %>% 
  filter(group == "Crustaceans") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Crustaceans_threat_percentages.png")

# Birds
data %>% 
  filter(group == "Birds") %>% 
  ggplot( aes(x = hsize, y = per, fill = cat)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

ggsave("figures/Birds_threat_percentages.png")
