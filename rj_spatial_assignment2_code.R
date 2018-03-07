

## Soc 413: Assignment 2
## Rebecca Johnson


################################# STEP ZERO: LOAD PACKAGES AND FUNCTIONS ##################

## load libraries and packages
library(sp)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)


## function to create cuts
## that create divergent color scale
## SOURCE: https://rstudio-pubs-static.s3.amazonaws.com/958_d3123f6a9f95436a8177dd096ad768a7.html 
symRangeCuts <- function(x, n, params = NA) {
  x.bar <- mean(x, na.rm = TRUE)
  cuts.list <- rangeCuts(c(-x + x.bar, x - x.bar), n) + x.bar
  return(cuts.list)
}

## function to check projections
## of shape files to make sure
## that they're the same 
check_and_change_projections <- function(list_ofobj){
  
  ## load associated libraries
  library(sp)
  library(rgdal)
  
  ## extract projections from list of sp objects
  list_ofproj <- lapply(list_ofobj, function(x) x@proj4string)
  
  ## returns index/indices of projections that differ in pairwise checking
  index_diffproj <- which(sapply(list_ofproj, FUN = identical,
                                 list_ofproj[[1]]) == FALSE)
  print(paste("indices that differ", paste(index_diffproj,
                                           collapse = ","),
              sep = ": "))
  
  ## returns index/indices of projections that don't differ in pairwise
  ## checking
  index_sameproj <- which(sapply(list_ofproj, FUN = identical,
                                 list_ofproj[[1]]) == TRUE)
  print(paste("indices that don't differ", paste(index_sameproj,
                                                 collapse = ","),
              sep = ": "))
  
  ## if any indices differ
  if(length(index_diffproj) > 0){
    
    print("some projections differ!")
    new_obj <- list_ofobj
    
    for(i in index_diffproj){
      new_obj[[i]] <- spTransform(new_obj[[i]], 
                                  CRSobj = new_obj[[min(index_sameproj)]]@proj4string) 
    }
    
    return(new_obj)
    
  } else {
    
    print("no projections differ!")
    return(list_ofobj)
  }
} 


########################## STEP ONE: LOAD SHAPEFILES ############
########################## AND RESTRICT TO NJ #################


## Unzip the district shapefiles--returns three sets of files
## 1. unified
## 2. elementary
## 3. secondary
district_shape <- grep("shape", list.files(), value = TRUE)
unzip(zipfile=district_shape)

setwd("nhgis0001_shape/")

## unzip each of the 
## three district types
all_district_shape = grep("shape", list.files(), value = TRUE)
for(i in 1:length(all_district_shape)){
  one_level = all_district_shape[i]
  unzip(zipfile=one_level)
}


## load the three district types
elm_shape = gsub("\\.zip", "", grep("elm.*zip", list.files(),
                 value = TRUE))
sec_shape = gsub("\\.zip", "", grep("sec.*zip", list.files(),
                                    value = TRUE))
uni_shape = gsub("\\.zip", "", grep("uni.*zip", list.files(),
                                    value = TRUE))

## create the three layer names 
all_layer = grep("\\.shp$", list.files(), value = TRUE)
elm_layer = gsub("\\.shp$", "", grep("elm", all_layer, value = TRUE))
sec_layer = gsub("\\.shp$", "", grep("sec", all_layer, value = TRUE))
uni_layer = gsub("\\.shp$", "", grep("uni", all_layer, value = TRUE))

## create list for each district unit of analysis
elm_shapelayer = list(elm_shape, elm_layer)
sec_shapelayer = list(sec_shape, sec_layer)
uni_shapelayer = list(uni_shape, uni_layer)

## read in shape files
elm_dist = readOGR(dsn=".", layer=elm_shapelayer[[2]],
                   stringsAsFactors=FALSE, encoding="latin1")
sec_dist = readOGR(dsn=".", layer=sec_shapelayer[[2]],
                   stringsAsFactors=FALSE, encoding="latin1")
uni_dist = readOGR(dsn=".", layer=uni_shapelayer[[2]],
                   stringsAsFactors=FALSE, encoding="latin1")


## put results in a list to be able to perform same operation on
## each
dist_alltypes_list= list(elm_dist, sec_dist, uni_dist)


## check to see if any of the projections differ
check_projchange <-  check_and_change_projections(list_ofobj = 
                      dist_alltypes_list)


## no projections differ so stick with normal list

## restrict school districts 
## to new jersey (STATEFP code == 34)
dist_alltypes_nj = list(length = 3)
for(i in 1:length(dist_alltypes_list)){
  one_type = dist_alltypes_list[[i]]
  dist_alltypes_nj[[i]] = one_type[one_type$STATEFP %in% "34", ]
}


## now have the polygon and other
## data for the school districts

########################## STEP TWO: MERGE SHAPEFILES ##########
####################### FOR ELEMENTARY AND UNIFIED DISTRICTS #####

## maybe just join the elementary and unified school districts
## needs the rownames-- gives an error 
## that names dont match initially, so changed
## some names of data
do_names_differ = setdiff(names(dist_alltypes_nj[[3]]),
                       names(dist_alltypes_nj[[1]]))
do_names_differ

## use gsub to remove prefix
names(dist_alltypes_nj[[1]]) <- gsub("ELS", "", names(dist_alltypes_nj[[1]]))
names(dist_alltypes_nj[[3]]) <- gsub("UNS", "", names(dist_alltypes_nj[[3]]))

dist_elem_unified =  maptools::spRbind(dist_alltypes_nj[[1]],
                                      dist_alltypes_nj[[3]]) 




######################## STEP THREE: ADD IN HH INCOME AND PLOT #############

## set wd to one containing demographic data
setwd("../dem_data/")
elem_dem = read.csv("nhgis0001_ds195_20095_2009_sd_elm_E.csv")
uni_dem = read.csv("nhgis0001_ds195_20095_2009_sd_uni_E.csv")

## subset each to nj and retain the 
## important variables: median hh income
##, and the gisjoin ID
elem_dem_nj = elem_dem %>%
          filter(STATE == "New Jersey") %>%
          dplyr::select(GISJOIN, RNHE001) %>%
          rename(med_hh_inc = RNHE001) %>%
  mutate(med_hh_inc_1000s = as.numeric(med_hh_inc/1000))


## add to data slot of 
## GIS object


uni_dem_nj = uni_dem %>%
  filter(STATE == "New Jersey") %>%
  dplyr::select(GISJOIN, RNHE001) %>%
  rename(med_hh_inc = RNHE001) %>%
  mutate(med_hh_inc_1000s = as.numeric(med_hh_inc/1000))




## bind into a single data.frame befrore merging
elemuni_dem_nj = rbind.data.frame(elem_dem_nj,
                                  uni_dem_nj)

## now merge with the main dataframe
## using the gisjoin id
dist_elem_unified_hhinc <- merge(dist_elem_unified, elemuni_dem_nj,
              by = "GISJOIN")


## recode the district (a 'water' district) missing to mean
## across the non-missing
dist_elem_unified_hhinc@data$med_hh_inc_1000s[is.na(dist_elem_unified_hhinc$med_hh_inc_1000s)] <- 
            mean(dist_elem_unified_hhinc@data$med_hh_inc_1000s,
                 na.rm = TRUE) 

## create shading scheme
income_shades <-auto.shading(dist_elem_unified_hhinc@data$med_hh_inc_1000s, n = 8, 
                    cutter = symRangeCuts, 
                        cols = brewer.pal(8, "RdYlGn"))

## plot boundaries of NJ district map
par(mar = c(1, 1, 4, 1))
plot(dist_elem_unified_hhinc, lwd = 0.25)
#title("Boundaries:\nNJ elementary and unified districts",
 #     cex = 0.9)


## shade districts by median household income
par(mar = c(1, 1, 1, 1))
choropleth(dist_elem_unified_hhinc, dist_elem_unified_hhinc$med_hh_inc_1000s,
           income_shades)
choro.legend(1650000, 530000,
             income_shades, fmt = "%4.1f", title = "Median household income\n($1000s; 2009 dollars)",
             bty = "n",
             cex = 0.8)
title("Median household income:\nNJ elementary and unified districts",
      cex = 0.9)


####################### STEP FOUR: ADD IN IEP RATE ###########
###################### AND PLOT ################################

## merge in data on disability rates
## by district using the LEAID
## from the NCES data explorer
setwd("../iep_data/")
iep_data <- read.csv("iepcount_clean.csv", header = TRUE,
                     stringsAsFactors = FALSE,
                     colClasses = "character") 



## create percentage of 
## students placed after
## coding districts without 
## count to mean across districts
iep_data <- iep_data %>%
          mutate(count_iep = as.numeric(count_iep),
            count_students = as.numeric(count_students),
            iep_rate = (count_iep/count_students) * 100,
            iep_rate_missing = ifelse(count_students < 0 | 
                                      count_iep < 0 |
                                      iep_rate > 100, 
                                      1, 0)) 


## merge with the spatial polygons data based
## on LEAID

## look at intersection between didfp and nces
overlap_id <- intersect(iep_data$ncesid,
                        dist_elem_unified_hhinc@data$DIDFP)

## see how many overlap
length(overlap_id)

## now merge
dist_elem_unified_hhinc_iep <- merge(dist_elem_unified_hhinc, 
                                     iep_data,
                                 by.x = "DIDFP",
                                 by.y = "ncesid",
                                 all.x = TRUE)


## create a factor variable
## indicating missing data

## set the iep_rate to NA if missing
dist_elem_unified_hhinc_iep$iep_rate[dist_elem_unified_hhinc_iep$iep_rate_missing == 1 |
                                    is.na(dist_elem_unified_hhinc_iep$iep_rate_missing)] <- NA


## assign missing to mean rate
dist_elem_unified_hhinc_iep$iep_rate[is.na(dist_elem_unified_hhinc_iep$iep_rate)] <- 
              mean(dist_elem_unified_hhinc_iep$iep_rate, na.rm = TRUE)

summary(dist_elem_unified_hhinc_iep$iep_rate)

## add binary indicator variable
dist_elem_unified_hhinc_iep$iep_rate_abovemedian = ifelse(dist_elem_unified_hhinc_iep$iep_rate >
                                                    median(dist_elem_unified_hhinc_iep$iep_rate),
                                                    1, 0)

table(dist_elem_unified_hhinc_iep$iep_rate_abovemedian)


## get coordinates and add 
## variable indicating 
## different point types for
## above or below state median
## coordinates(dist_elem_unified_hhinc_iep)
coordinates_iep_map = as.data.frame(coordinates(dist_elem_unified_hhinc_iep))
colnames(coordinates_iep_map) <- c("x", "y")
coordinates_iep_map$iep_median <- dist_elem_unified_hhinc_iep$iep_rate_abovemedian
coordinates_iep_map$point_type = ifelse(coordinates_iep_map$iep_median == 1,
                                        16, 6) 


par(mar = c(1, 1, 5, 1),
    mfrow = c(1, 1))
choropleth(dist_elem_unified_hhinc_iep, dist_elem_unified_hhinc_iep$med_hh_inc_1000s,
           income_shades)
points(coordinates_iep_map,
       pch = coordinates_iep_map$point_type,
       cex = 0.5,
       col = 'black')  # then change shape based on median
choro.legend(1580000, 530000,
             income_shades, fmt = "%4.1f", title = "Median household income\n($1000s; 2009 dollars)",
             bty = "n",
             cex = 0.8)
legend(1580000, 580000, c("IEP rate > state median", "IEP rate <= state median"), 
       pch = c(16, 6),
       cex = 0.8,
       bty = "n",
       title = "Rate of district\nproviding contracts\nfor disability services\n(2008-2009 school year)")
#title("Relationship between median household income\nand provision of disability services:\nNJ elementary and unified districts",
 #     cex = 0.9)



## run regression
## to see if visualization failed
## to pick up actual bivariate relationship
summary(glm(iep_rate_abovemedian ~ med_hh_inc_1000s, 
    data = dist_elem_unified_hhinc_iep@data,
    family = "binomial"))

