library(rvest)

min_forecast <- function(gage, datum){
  url <- paste0('https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=',
                gage,
                'v2&output=tabular&time_zone=est')
  
  forecast <- url |> 
    read_html() |> 
    html_element(xpath = '/html/body/table/tr/td[2]') |> 
    html_table()
  
  if(datum == 'stage'){
    return(
      min(as.numeric(gsub('ft', '', forecast$X2[-(1:2)])))
    )
  }
  if(datum == 'flow'){
    return(
      min(as.numeric(gsub('kcfs', '', forecast$X3[-(1:2)])))
    )
  }
  
}

# min_forecast('rmd', 'stage')
# min_forecast('hrk', 'flow')
