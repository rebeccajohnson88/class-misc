

## Week 3 activity

## first step is to check projections
## in sp-type map objects

## function to check if projection differs
## and change to match others (might be too specific to present example)


## function
## 1. takes in: list of sp objects
## steps:
## 1. extracts projections from those objections
## 2. checks which projections differ from the other
## and separates indexes for projections into
## ones that differ and ones that don't
## 3. if any projections differ,
## 3.1 creates a new list of spatial polygons (new_obj),
## 3.2 for all projections that differ, updates the 
## projection to the projection of one that does not
## differ (just takes the minimum index of those that 
## do not differ, but this is arbitrary)
## 3.3. return the new list of sp objects
## 4. if no projections differ, just returns 
## the same list of sp objects we fed the function

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

                        
                        
#### Example application:
## load sp objects used in exercise
##load("Soc413_Week3_Wednesday.RData")

## create a list to feed the function
## list_wk3 <- list(block, tract, city, library)
## newlist_wk3 <-  check_and_change_projections(list_ofobj = list_wk3)

## check projections the same after we apply the function
## test_check <- check_and_change_projections(newlist_wk3)

