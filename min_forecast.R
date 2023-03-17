library(rvest)
library(tidyr)
library(ggplot2)
library(blastula)
library(gt)

scrape_gage <- function(gage){
  url <- paste0('https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=',
                gage,
                'v2&output=tabular&time_zone=est')
  
  forecast <- url |> 
    read_html() |> 
    html_element(xpath = '/html/body/table/tr/td[2]') |> 
    html_table()
}

forecast <- scrape_gage('rmd')
forecast <- forecast[-(1:2),]
forecast$date <- as.POSIXct(forecast$X1, format = '%m/%d %H:%M')
forecast$stage <- as.numeric(gsub('ft', '', forecast$X2))
forecast$flow <- as.numeric(gsub('kcfs', '', forecast$X3))
forecast <- forecast[, 4:6]


forecast <- pivot_longer(forecast, cols = c('flow', 'stage'))
limits <- data.frame(name = c('flow', 'stage'),
                     limit = c(5.5, 5))




create_smtp_creds_file('test_creds_file',
                       'mikeob9@gmail.com',
                       provider = 'gmail')

if(min(forecast[forecast$name == 'stage', 'value']) < 5.5){
compose_email(
  body = md(
    c(
      'Richmond-Westham is predicted to hit a minimum of ',
      {min(forecast[forecast$name == "stage", "value"])},
      ' feet in the next three days.\n',
      {forecast |>
          pivot_wider(id_cols = date)|> 
          gt() |> 
          as_raw_html()},
      add_ggplot(ggplot(data = forecast) +
                   geom_line(aes(x = date, y = value)) +
                   geom_hline(data = limits,
                              aes(yintercept = limit), color = 'red')+
                   facet_wrap(~name, ncol = 1) +
                   labs(x = NULL, y = 'Value') +
                   theme_minimal())
    )
  )
) |> 
    smtp_send(
      to = 'obrien@umces.edu',
      from = c("GitHub Actions Script Robot" = "mikeob9@gmail.com"),
      subject = 'Get ready to go sampling!',
      credentials = {
        hold <- jsonlite::unserializeJSON(Sys.getenv('CRED_FILE'))
        class(hold) <- c("creds_file", "blastula_creds")
        hold
      }
    )
}
