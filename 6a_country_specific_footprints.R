#######################################################
###------ Continent-specific footprints ----------#####
#######################################################

# create output df
FP_country <- data.frame(Country_ID = unique(FP$country),
                        Country = rep(NA, length(unique(FP$country))),
                         SQ_biomass = rep(NA, length(unique(FP$country))),
                         DGE_biomass = rep(NA, length(unique(FP$country))),
                         lancet_biomass = rep(NA, length(unique(FP$country))),
                         EATveg_biomass = rep(NA, length(unique(FP$country))),
                         SQ_landuse = rep(NA, length(unique(FP$country))),
                         DGE_landuse = rep(NA, length(unique(FP$country))),
                         lancet_landuse = rep(NA, length(unique(FP$country))),
                         EATveg_landuse = rep(NA, length(unique(FP$country))), 
                         SQ_water = rep(NA, length(unique(FP$country))),
                         DGE_water = rep(NA, length(unique(FP$country))),
                         lancet_water = rep(NA, length(unique(FP$country))),
                         EATveg_water = rep(NA, length(unique(FP$country)))
                         )
FP_country$Country <- countries$Country[match(FP_country$country_ID, countries$ISO)] # add column with complete name of country

Y_list <- list(Y_SQ, Y_DGE, Y_lancet, Y_EATveg)

##### BIOMASS ######
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed
rm(L)
rm(E)

for (i in 1:length(Y_list)){
  FP_tot <- t(t(MP) * Y_list[[i]])
  FP <- data.frame(country = index$country,
                 value = rowSums(FP_tot))
  FP_country[,(i+2)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin
}



##### LANDUSE ######
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
e <- E$Landuse / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed
rm(L)
rm(E)

for (i in 1:length(Y_list)){
  FP_tot <- t(t(MP) * Y_list[[i]])
  FP <- data.frame(country = index$country,
                   value = rowSums(FP_tot))
  FP_country[,(i+6)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin
}


##### WATER ######
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
e <- E$Blue_water / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed
rm(L)
rm(E)

for (i in 1:length(Y_list)){
  FP_tot <- t(t(MP) * Y_list[[i]])
  FP <- data.frame(country = index$country,
                   value = rowSums(FP_tot))
  FP_country[,(i+10)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin
}

write.csv2(FP_country, file = "output/FP_countries.csv")


#######################################################
###------ Continent-specific footprints ----------#####
#######################################################

FP_Continent <- data.frame(continent = unique(FP$continent),
                          SQ = rep(NA, length(unique(FP$continent))),
                          DGE = rep(NA, length(unique(FP$continent))),
                          lancet = rep(NA, length(unique(FP$continent))),
                          plantbased = rep(NA, length(unique(FP$continent))),
                          EATveg = rep(NA, length(unique(FP$continent))))


Y_tot <- Y_EATveg  # choose scenario
FP_tot <- t(t(MP) * Y_tot)

FP <- data.frame(country = index$country,
                continent = index$continent,
                value = rowSums(FP_tot))
FP <- aggregate(value ~country + continent, FP, sum) # footprint pro ursprungsland

# for EACH FP (footprint and scenario), fill in the data table!
for (i in 1:nrow(FP_Continent)){
 FP_Continent$SQ[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}

for (i in 1:nrow(FP_Continent)){
 FP_Continent$DGE[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}

for (i in 1:nrow(FP_Continent)){
  FP_Continent$lancet[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}
for (i in 1:nrow(FP_Continent)){
FP_Continent$plantbased[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}

for (i in 1:nrow(FP_Continent)){
 FP_Continent$EATveg[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}


write.csv2(FP_Continent, file = "output/FP_Landuse_continents.csv")


########## notes ################

# FP_plant <- sum(colSums(MP) * Y_plant) #
# 
# create result matrix
# NrOfProducts <- 130
# FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                         products = rep(index$product, each = NrOfProducts),
#                         product_group = rep(index$product_group, each = NrOfProducts))

