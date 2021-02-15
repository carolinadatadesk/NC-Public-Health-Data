get_LHD_page <- function(county, lhd){
  rmarkdown::render(
    "LHD_reports.Rmd",
    params = list(county = county, lhd = lhd),
    output_file = paste0(county, ".html")
  )
}

library(purrr)
walk2(pagedata$county_name, pagedata$lhd_name, get_LHD_page)
