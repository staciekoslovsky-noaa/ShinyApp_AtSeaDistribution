source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/helper_functions.R')
# source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/app.R')


#load_all_files(urls)

species_links <- list(
  "Northern Minke Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BA_MCMC.RData',
  "Fin Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData',
  "Northern Fur Seal" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/CU_MCMC.RData',
  "Steller Sea Lion" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EJ_MCMC.RData',
  "Sea Otter" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EL_MCMC.RData',
  "Gray Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/ER_MCMC.RData',
  "Pacific White-Sided Dolphin" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/LO_MCMC.RData',
  "Humpback Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/MN_MCMC.RData',
  "Killer Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OO_MCMC.RData',
  "Walrus" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OR_MCMC.RData',
  "Dall's Porpoise" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PD_MCMC.RData',
  "Sperm Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PM_MCMC.RData',
  "Harbor Porpoise" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PP_MCMC.RData',
  "Harbor Seal" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PV_MCMC.RData'
)

# New dataframe containing the selected MCMC data (abundance calculations source)
# with the POPhexagons data (spatial data source) 

row_variances <- apply(RelAbund_MCMC, 1, var)

POPdata_with_MCMC <- cbind(POPhex_MCMC, RelAbund_MCMC, row_variances)
 

# Move across dateline
POPdata_with_MCMC$geometry <- (sf::st_geometry(POPdata_with_MCMC) + c(360, 90)) %% c(360) - c(0, 90)

# # Set centroid values
POPdata_with_MCMC$centroid.x <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,1]
POPdata_with_MCMC$centroid.y <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,2]
# 
# # Get
# POPdata_with_MCMC$centroid.x <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,1]
#POPdata_with_MCMC$centroid.y <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,2]

load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData'))

total_abundance_samples <- colSums(RelAbund_MCMC)

ggplot(data.frame(TotalAbundance = total_abundance_samples), aes(x = TotalAbundance)) +
  geom_histogram(binwidth = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  ggtitle("Histogram of MCMC Samples for Total Abundance") +
  xlab("Total Abundance") +
  ylab("Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
