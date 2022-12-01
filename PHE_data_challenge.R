# rm(list=ls()) # Clear variables
graphics.off() # Close figures


library(neonUtilities)
library(ggplot2)
# library(plotly)



### Inputs

siteid <- "SCBI"
clear.cache <- F # TRUE to re-download PHE data for sites already loaded





# Import functions (h/t Clockwork)
functions <- list.files(file.path('functions'), pattern = "*.R$", full.names = T, ignore.case = T)
invisible(sapply(functions, source, .GlobalEnv))


## Load PHE portal data

# Simple cache
if(!exists("phe.cache") | clear.cache)
  phe.cache <- data.frame()
if(!siteid %in% phe.cache$siteID)
  phe.cache <- rbind(getPortal_PHE_status(siteid), phe.cache)

## All phe obs data
phe <- phe.cache[phe.cache$siteID==siteid,]

## Create table per species, per date
phe.spp <- unique(phe[,c("date", "dayOfYear", "taxonID", "scientificName", "taxonRank", "growthForm")])

# Vector of extant phenophases
phephases <- unique(phe$phenophaseName)

# Initialize columns per extant phenophase
for(phephase in phephases)
  phe.spp[,phephase] <- NA

rm(phephase)

# Observation year
phe.spp$year <- substr(phe.spp$date, 0, 4)

## Tally 'yes' status per species, per date
# Develop 'apply' method if time; this method is time consuming
writeLines(c("", "Tallying per-date status. Processing may take up to 30 seconds..."))

for(i in 1:nrow(phe.spp)){
  
  phe.i <- phe[phe$taxonID == phe.spp$taxonID[i]
               & phe$date == phe.spp$date[i],]
  
  for(phephase in phephases){
    
    phephase.i <- phe.i[phe.i$phenophaseName == phephase,]
    
    if(nrow(phephase.i) > 0)
      phe.spp[,phephase][i] <- sum(phephase.i$phenophaseStatus == "yes", na.rm = T)
  }
}

rm(phephase)

## Plot n = "yes" per phenophase, faceted per species, colored by year

plot_phenoPhase <- function(siteid, df, phephase){

  df <- df[!is.na(df[,phephase]),]
  
  title.plot <- paste0(phephase, ", ", siteid, ", ", min(df$year), " to ", max(df$year))
  
  p <- ggplot(df, aes(x = dayOfYear, y = df[,phephase])) +
    geom_line(aes(color = year), alpha = 0.5, size = 1) +
    facet_wrap(~ taxonID) +
    labs(title = title.plot, x = "Day of Year", y = phephase)
  
  print(p)
  
}

for(phephase in phephases)
  plot_phenoPhase(siteid = siteid, df = phe.spp, phephase = phephase)

# pl <- ggplotly(p)
# print(pl)
