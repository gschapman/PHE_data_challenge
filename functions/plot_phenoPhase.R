
plot_phenoPhase <- function(siteid, df, phephase){
  
  df <- df[!is.na(df[,phephase]),]
  
  title.plot <- paste0(phephase, ", ", siteid, ", ", min(df$year), " to ", max(df$year))
  
  p <- ggplot(df, aes(x = dayOfYear, y = df[,phephase])) +
    geom_line(aes(color = year), alpha = 0.5, size = 1) +
    facet_wrap(~ taxonID) +
    labs(title = title.plot, x = "Day of Year", y = paste("n", phephase, "status = 'yes'"))
  
  print(p)
  
}