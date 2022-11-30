
# rm(list=ls()) # Clear variables
graphics.off() # Close figures


library(neonUtilities)



### Inputs

siteid <- "SERC"
clear.cache <- F # TRUE to re-download PHE data for sites already loaded





# Import functions (h/t Clockwork)
functions <- list.files(file.path('functions'), pattern = "*.R$", full.names = T, ignore.case = T)
invisible(sapply(functions, source, .GlobalEnv))


## Load PHE portal data

# Simple cache
if(!exists("phe.cache") | clear.cache) phe.cache <- data.frame()
if(!siteid %in% phe.cache$siteID)
  phe.cache <- rbind(getPortal_PHE_status(siteid), phe.cache)


## All phe obs data
phe <- phe.cache[phe.cache$siteID==siteid,]

## Create table per species, per date
phe.spp <- unique(phe[,c("date", "dayOfYear", "taxonID", "scientificName", "taxonRank", "growthForm")])


phephases <- unique(phe$phenophaseName)

# Initialize columns per extant phenophase
for(phephase in phephases)
  phe.spp[,phephase] <- NA

# phe.spp$year <- NA

phe.spp$year <- substr(phe.spp$date, 0, 4)

## Tally 'yes' status per species, per date
# Develop 'apply' method if time; this method is time consuming
writeLines(c("", "Tallying per-date status. Processing may take up to 30 seconds..."))

for(i in 1:nrow(phe.spp)){
  
  # phe.spp$year[i] <- substr(phe.spp$date, 0, 4)
  
  phe.i <- phe[phe$taxonID == phe.spp$taxonID[i]
               & phe$date == phe.spp$date[i],]
  
  for(phephase in phephases){
    
    phe.spp[,phephase][i] <- nrow(
      phe.i[phe.i$phenophaseName == phephase
            & phe.i$phenophaseStatus == "yes",]
    )
  }
}


## Plot n = "yes" per phenophase, faceted per species, colored by year
df <- phe.spp

p <- ggplot(df, aes(x = dayOfYear, y = df[,"Breaking leaf buds"])) + # , group=year
  geom_line(aes(color=year), alpha=0.5) +
  facet_wrap(~ taxonID)

print(p)
