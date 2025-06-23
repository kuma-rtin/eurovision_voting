library(ISOcodes)

# Cleans up the country names to have standardized names
clean_countryName = function(cntry) {
  if (grepl("Czech", cntry, ignore.case=TRUE)) {
    return("Czechia")
  }
  if (grepl("Kingdom", cntry, ignore.case=TRUE)) {
    return("United.Kingdom")
  }
  if (grepl("Netherlands", cntry, ignore.case=TRUE)) {
    return("Netherlands")
  }
  if (grepl("Marino", cntry, ignore.case=TRUE)) {
    return("San.Marino")
  }
  if (grepl("Macedonia", cntry, ignore.case=TRUE)) {
    return("North.Macedonia")
  }
  if (grepl("Roma", cntry, ignore.case=TRUE)) {
    return("Romania")
  }
  if (grepl("Bosnia", cntry, ignore.case=TRUE)) {
    return("Bosnia.Herzeg")
  }
  if (grepl("Serbia...Mont", cntry, ignore.case=TRUE)) {
    return("Serbia.Monten")
  }
  return(cntry)
}

# Cleans a dataframe: standardizes the country names and orders the rows and columns
clean_df = function(df) {
  # clean col and row names
  colnames(df) = lapply(colnames(df), clean_countryName)
  rownames(df) = lapply(rownames(df), clean_countryName)
  
  # order the cols and rows by country
  df = df[order(rownames(df)),order(colnames(df))]
  
  return(df)
}

# Loads a csv table as a dataframe from the ./data/ folder
load = function(name) {
  df = clean_df(read.csv(sprintf("./data/%s", name), row.names = 1))
  return(df)
}

# Loads the ESC data (1975-2015 from Mantzaris et al. (2018) and from 2016
# onwards the custom scraped data split into jury and televotes)
load_all = function() {
  # Load all dataframes
  dfs = sapply(sprintf("mantzaris/%s.csv", 1975:2015), load)
  
  # Data from 2016 onwards is split into jury and televoting
  dfs_j = sapply(paste(c(2016:2019, 2021:2023), "_jury.csv", sep=""), load)
  dfs_p = sapply(paste(c(2016:2019, 2021:2022), "_public.csv", sep=""), load)
  
  # Add public votes of 2023 separately, because we are removing the rest of the world vote
  df_2023 = load("2023_public.csv")
  df_2023 = df_2023[(row.names(df_2023) != "Rest of the World"),]
  dfs_p = c(dfs_p, list(df_2023))
  
  # Get means and append to main dataframe
  dfs_a = mapply(function(x, y) return((x+y)/2), dfs_j, dfs_p)
  dfs = c(dfs, dfs_a)
  dfs = setNames(dfs, paste("y", c(1975:2019, 2021:2023), sep=""))
  rm(dfs_a)
  rm(dfs_j)
  rm(dfs_p)
  return(dfs)
}

# Returns all participants of the ESC data in dfs in a specified amount of time
get_countryNames = function(dfs, startYear=1975, endYear=2023) {
  ys = if (! 2020 %in% startYear:endYear) startYear:endYear else c(startYear:2019, 2021:endYear)
  years = dfs[paste("y", startYear:endYear, sep="")]
  countryNames = unique(unlist(lapply(years, rownames)))
  return(sort(countryNames))
} 

# Returns all the votes given by country1 to country 2 over a given period of time
# with NA values for years where they did not participate together
get_givenVotes = function(country1, country2, dfs, startYear, endYear) {
  getVote = function(y) {
    if(country1 %in% rownames(y) & country2 %in% colnames(y)) {
      return(y[country1, country2])
    }
    return(NA)
  }
  
  # There was no ESC in the year of 2020
  ys = if (! 2020 %in% startYear:endYear) startYear:endYear else c(startYear:2019, 2021:endYear)
  years = dfs[paste("y", ys, sep="")]
  
  givenVotes = unlist(lapply(years, getVote))
  names(givenVotes) = ys
  return(givenVotes)
}

# Returns a standardized pairname for two countries 
get_standardizedPairName = function(x, y) {
  return(paste(sort(c(x,y)), collapse="<=>"))
}

# Converts a standardized country name (standardized using the function 
# clean_countryName()) to a two letter ISO code
convert_toISO = function(country) {
  cntry = gsub(".", " ", country, fixed=TRUE)
  if (grepl("Bosnia", cntry, ignore.case=TRUE)) {
    cntry = "Bosnia and Herzegovina"
  } else if (country == "Moldova") {
    cntry = "Moldova, Republic of"
  } else if (country == "Russia") {
    cntry = "Russian Federation"
  } else if (country == "Serbia.Monten") {
    return("CS")
  } else if (country == "Yugoslavia") {
    return("YU")
  }
  
  return(ISO_3166_1[ISO_3166_1$Name == cntry,"Alpha_2"])
}
