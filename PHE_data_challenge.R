
# rm(list=ls()) # Clear variables
graphics.off() # Close figures


library(neonUtilities)



### Inputs

siteid <- "SCBI"
clear.cache <- F # TRUE to re-download PHE data for sites already loaded





# Import functions (h/t Clockwork)
functions <- list.files(file.path('functions'), pattern = "*.R$", full.names = T, ignore.case = T)
invisible(sapply(functions, source, .GlobalEnv))


## Load PHE portal data

# Simple cache
if(!exists("phe.cache") | clear.cache) phe.cache <- data.frame()
if(!siteid %in% phe.cache$siteID)
  phe.cache <- rbind(getPortal_PHE_status(siteid), phe.cache)

phe <- phe.cache[phe.cache$siteID==siteid,]