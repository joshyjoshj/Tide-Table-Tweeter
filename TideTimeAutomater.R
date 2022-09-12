#Packages
library(tidyverse)
library(lubridate)
library(kableExtra)
library(formattable)
library(rtweet)
library(magick)
library(webshot)
#Extracting Data
tides <- tibble(jsonlite::fromJSON("https://admiraltyapi.azure-api.net/uktidalapi/api/V1/Stations/0028/TidalEvents?duration=1&key=c162d26009634626b9b8c4bd17000e1c"))[c(1,2,4)]
#Cleaning Data
data <- tides%>%
  mutate(DateTime=hms::as_hms(with_tz(hms::as_hms(substr(DateTime,12,19)),tzone = "Etc/GMT-1")))%>%
  mutate(Height=round(Height,digits=2))%>%
  mutate(EventType = case_when(
    EventType == "HighWater"~"High Water",
    EventType == "LowWater"~"Low Water",
    TRUE~EventType
  ))%>%
  rename(Time = DateTime)%>%
  rename(Type=EventType)
#Adding HTML styling
data$Height <- color_bar("lightblue")(data$Height)
data$Type <- ifelse(
  {data$Type=="Low Water"},
  {cell_spec(data$Type,"html", background="green", color="white",align="center")},
  {cell_spec(data$Type,"html", background="red",color="white",align="center")}
)
#Final Data Clean
data <- data%>%rename("Height (m)" = "Height")
#creating Kable 
date <- format(Sys.Date(),format="%B %d %Y")
title <- as.character((paste("Tide Times for Lyme Regis on ", date)))
header <- data.frame(title,c(3:3))
table <- kable(data,format="html",escape=FALSE,full_width=F, table.attr = "style='width:30%;'" )%>%kable_classic(html_font = "Times")%>%kable_styling(bootstrap_options = "striped")%>%
  add_header_above(header)%>%footnote(general="Data from UKHO API")%>%column_spec(3,width="30em")%>%column_spec(1,width="15em")%>%column_spec(2,width="10em")
table
#Saving Kable
table %>% save_kable("testtidetable.jpeg",zoom = 8)
#store api keys 
api_key <- "q506XF49ZRSQX2vnSaHULmALB"
api_secret_key <- "XemeI1DhhSfK4ER5aHWrvKvvNgIpCIinhX5eAHoxWmmjzbXpdv"
access_token <- "1118102319583977472-i8BE1tmvqEWPoGsY36275lnDrh6ReS"
access_token_secret <- "7Mmcb7akLFfQJlC1Yz8WPGMVMmb2oRZ4ee9glzCkTP75Z"
#Authenticate via web browser
token <- create_token(
  app = "Tide Times Automater",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
#Composing Tweet
post_tweet(paste("If you or someone else is in difficulty at the coast dial #999 and ask for the Coastguard"), media = "testtidetable.jpeg",token=token)


