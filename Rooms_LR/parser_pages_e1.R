setwd("D:/GitHub/R_project/Rooms_LR")
library(stringr)
library(RCurl)

links <- read.csv("test.csv", sep=";", stringsAsFactors = FALSE, header = FALSE)
html <- getURLContent(links[1,2], .encoding = "utf-8")
html_split <- unlist(strsplit(html,"\r"))

p_address <- "<.*?\"card__address\">(.*?)<"
p_cost <- "<.*?\"card__cost\">(.*?)<"
p_cost_m2 <- "<.*?\"card__price\">(.*?)<"
p_cost_type <- "<.*?\"card__deal-type\">(.*?)<"

for (i in 1:length(html_split)) {
  r_cost <- str_match(html_split[i],p_cost)
  if (is.na(r_cost[1,1])==FALSE) cost <- r_cost[1,2]

  r_cost_m2 <- str_match(html_split[i],p_cost_m2)
  if (is.na(r_cost_m2[1,1])==FALSE) cost_m2 <- r_cost_m2[1,2]

  r_cost_type <- str_match(html_split[i],p_cost_type)
  if (is.na(r_cost_type[1,1])==FALSE) cost_type <- r_cost_type[1,2]
  
}


p_area_keys <- "\"sms-card-list__key\">(.*?)</dt.*?\"sms-card-list__value\">(.*?)<"
p_area <- "<div.*?\"sms-card-list\">(.*?)</div>"
r_area <- str_match(html,p_area)
if (is.na(r_area[1,1])==FALSE) area <- r_area[1,2]
c_area <- gsub("[\t\n]+","",area)
c_area <- gsub("\\s{1,}"," ",c_area)
r_area_keys <- str_match_all(c_area,p_area_keys)
r_area_keys

p_details_keys <- "\"key-value__key\">(.*?)</dt.*?\"key-value__value\">(.*?)<"
p_details <- "<div.*?\"card__details\">(.*?)</div>"
r_details <- str_match(html,p_details)
if (is.na(r_details[1,1])==FALSE) details <- r_details[1,2]
c_details <- gsub("[\t\n]+","",details)
c_details <- gsub("\\s{1,}"," ",c_details)
r_details_keys <- str_match_all(c_details,p_details_keys)
r_details_keys
