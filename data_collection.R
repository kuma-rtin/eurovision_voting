library(rvest)
library(plyr)
`%>%` <- dplyr::`%>%`

source("utils.R")

# Editions of ESC for which the points should be scraped
# from 2016 both, jury and public votes were published
LOCATIONS = c("Stockholm-2016", "Kyiv-2017", "Lisbon-2018", "Tel-Aviv-2019", "Rotterdam-2021", "Turin-2022", "Liverpool-2023")
# Points that each country can give to another one
POINTS = c(12, 10, 8:1)

# Set to TRUE if scraping should be performed
do_scrape = FALSE 

# Returns a list of countries that participated in the finals given a location and year 
# specified in the parameter l
get_final_cntrs = function(l) {
  pg = read_html(sprintf("https://eurovision.tv/event/%s/grand-final", l))
  c = html_elements(pg, xpath="//tbody/tr/td[3]") %>% html_text(trim=TRUE)
  c = unlist(lapply(c, clean_countryName))
  return(c)
}

# Get points for the years 2016-2022 
# There was no "Rest of the world vote" and not all countries had the "Who voted for country X" section
# Returns a list of points that the country specified in url gave to all other countries
# The first length(countries) points describe the jury votes, the second part
# is the public votes
get_points = function(url, countries) {
  # Get all the votes
  # The website contains public votes first and then jury votes ordered from 12-1 points
  # The order of the country names determines the number of points the country got
  # e.g. v[1] got 12 points from the public, v[2] got 10 etc. 
  # v[11] got 12 points from the jury, v[12] got 10 etc.
  v = read_html(url) %>% 
    html_elements(xpath="//div[@class='details-tab__voted-participant revert']//div[@class='details-tab__voter']") %>%
    html_text(trim=TRUE)
  
  # Split into jury and public votes and determine the points using v
  j = rep(0, length(countries))
  j[match(v[11:length(v)], countries)] = POINTS
  p = rep(0, length(countries))
  p[match(v[1:10], countries)] = POINTS
  
  return(c(as.integer(j), as.integer(p)))
}

# Get points for 2023 onwards
# In 2023 the "Rest of the world vote" was introduced. Therefore the "Who voted for country X" 
# section therefore has to be used to determine the points
# Returns a list of points that the country specified in url *received* from all other countries
# The first length(countries) points describe the jury votes, the second part
# is the public votes
get_points2023 = function(url, countries) {
  
  # Get the jury votes received by the country specified in url from all other countries
  # The website containes a table of the votes received 
  # E.g. the table cell for the value 12 containes all the countries that awarded the country
  # specified in url with 12 points
  vj = read_html(url) %>%
    html_elements(pg, xpath="//div[@class='details-tab__voted-for-participant']/div[@class='views-element-container'][2]//div[@class='details-tab__voted-row']") 
  j = rep(0, length(countries))
  # Get the name of the country that gave the points specified in vj and save in the matrix
  for (i in 1:length(vj)) {
    pt = html_element(vj[i], "div") %>% html_text()
    cntrs = html_elements(vj[i], xpath="div[@class='details-tab__voter']") %>% html_text(trim=TRUE)
    j[match(cntrs, countries)] = pt
  }
  
  # Get the public votes received by the country specified in url from all other countries
  vp = html_elements(pg, xpath="//div[@class='details-tab__voted-for-participant']/div[@class='views-element-container'][1]//div[@class='details-tab__voted-row']") 
  p = rep(0, length(countries))
  for (i in 1:length(vp)) {
    pt = html_element(vp[i], "div") %>% html_text()
    cntrs = html_elements(vp[i], xpath="div[@class='details-tab__voter']") %>% html_text(trim=TRUE)
    p[match(cntrs, countries)] = pt
  }
  return(c(as.integer(j), as.integer(p)))
}

if (do_scrape) {
  for (l in LOCATIONS) {
    year = as.numeric(gsub("\\D", "", l))
    # Parse the current edition's results page 
    url = sprintf("https://eurovision.tv/event/%s/grand-final/results", l)
    parsed_page = read_html(url)
    
    # Get all the countries that voted and standardize their names
    countries = html_elements(parsed_page, "option") %>% html_text()
    clean_countries = lapply(countries, clean_countryName)
    final_cntrs = get_final_cntrs(l)
    
    # Get the urls for all the detailed voting page for every country
    country_urls = html_elements(parsed_page, "option") %>% html_attr("value")
    
    # Get points for every country
    if(year >= 2023) {
      # Year 2023 has to be treated differently
      countries = c(countries, "Rest of the World")
      clean_countries = c(clean_countries, "Rest of the World")
      pts = lapply(country_urls, get_points2023, countries)
    } else {
      pts = lapply(country_urls, get_points, countries)
    }
    
    # Save data to csv using the same format as mantzaris:
    # Country in row X gave points to the country in column Y in a certain year 
    pts_matrix = do.call(rbind,pts)
    jury = as.data.frame(pts_matrix[,1:length(clean_countries)], row.names=clean_countries)
    if (year >= 2023) {
      # Matrix needs to be transposed as it contains the points a country in row X received 
      # from a country in column Y
      jury = t(jury)
      # Delete Rest of the world row because irrelevant for jury votes
      jury = jury[-nrow(jury),]
      rownames(jury) = clean_countries[-length(clean_countries)]
      colnames(jury) = clean_countries[-length(clean_countries)]
    } else {
      colnames(jury) = clean_countries
    }
    # Only keep the columns of countries that participated in the finals
    jury = clean_df(jury[,final_cntrs])
    write.csv(jury, file=sprintf("./data/%s_jury.csv", year))
    
    public = as.data.frame(pts_matrix[,-(1:length(clean_countries))], row.names=clean_countries)
    if (year >= 2023) {
      # Matrix needs to be transposed as it contains the points a country in row X received 
      # from a country in column Y
      public = t(public)
      rownames(public) = clean_countries
      # Columns do not include "Rest of the World" 
      colnames(public) = clean_countries[-length(clean_countries)]
    } else {
      colnames(public) = clean_countries
    }
    # Only keep the columns of countries that participated in the finals
    public = clean_df(public[,final_cntrs])
    write.csv(public, file=sprintf("./data/%s_public.csv", year))
  }
}

# Clean-up
rm(LOCATIONS)
rm(POINTS)
rm(do_scrape)



