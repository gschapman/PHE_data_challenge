
## Get PHE portal status intensity data

getPortal_PHE_status <- function(siteid){
  
  phe <- loadByProduct(
    dpID = "DP1.10055.001",
    site = siteid,
    startdate = "2017-01", # Only data post-Fulcrum
    enddate = "2022-11",
    check.size = F
  )
  
  ## Process phe_perindividual
  phe.ind <- phe[["phe_perindividual"]]
  
  # Keep most recent per dpug if dups exist
  # Sort by ID and editedDate, remove duplicates (leaving last value i.e. most recent version)
  phe.ind <- phe.ind[with(phe.ind, order(individualID, editedDate)),]
  phe.ind <- phe.ind[!duplicated(phe.ind$individualID, fromLast = T),]
  
  
  ## Processs phe_statusintensity
  phe.stat <- phe[["phe_statusintensity"]]
  
  # Keep most recent per dpug if dups exist
  # Sort by ID, phenophaseName, date, and editedDate, remove duplicates
  phe.stat <- phe.stat[with(phe.stat, order(individualID, phenophaseName, date, editedDate)),]
  phe.stat <- phe.stat[!duplicated(phe.stat[,c("individualID", "phenophaseName", "date")], fromLast = T),]
  
  
  ## Merge relevant perindividual data
  phe.stat.ind <- merge(
    phe.stat,
    phe.ind[,c("individualID", "transectMeter", "directionFromTransect", "ninetyDegreeDistance",
               "taxonID", "scientificName", "identificationQualifier", "taxonRank", "growthForm")],
    all.x = T
  )
  
  # Check - clear for SCBI
  # any(is.na(phe.stat.ind$growthForm))
  
  return(phe.stat.ind)
  
}