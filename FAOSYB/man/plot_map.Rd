\name{plot_map}
\alias{plot_map}
\title{A function for plotting choropleth map}
\usage{
  plot_map(shpFile, var, data, countryCode = "FAOST_CODE",
    n = 5, style = "jenks", manualBreaks,
    col = c("#F5F5F5", "#C8E2DE", "#9CCFC7", "#70BCB0", "#44AA99"),
    missCol = "#8B8878", missLabel = "No data available",
    subset = TRUE, scale = 1,
    shpProj = "+proj=robin +ellps=WGS84",
    outProj = "+proj=robin")
}
\description{
  A function for plotting choropleth map
}

