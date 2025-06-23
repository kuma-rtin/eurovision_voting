library(plyr)
`%>%` <- dplyr::`%>%`

source("utils.R")

# Checks if the Mantzaris dataframe and the jury part of a scraped dataframe
# specified by a Eurovision year (y) are equal
check_dfs = function(y) {
  mantz = load(sprintf("mantzaris/%s.csv", y))
  mine = load(sprintf("%s_jury.csv", y))
  mine = clean_df(mine)
  
  if (!all(rowSums(mine) == 58)) {
    return(FALSE)
  }
  
  if (!all(colnames(mantz) %in% colnames(mine))) {
    return(FALSE)
  }
  return(all(mantz == mine))
}

# Compare all dataframes
sapply(c(2016:2019, 2021), check_dfs)

# Three dataframes are not correct: 2017, 2019 and 2021
# Check 2017
y = 2017
mantz = load(sprintf("mantzaris/%s.csv", y))
mine = load(sprintf("%s_jury.csv", y))
# Dataframe contains columns of countries that were not in the finals
all(mantz[,colnames(mine)] == mine)
# Now they are equal

# Check 2019
y = 2019
mantz = load(sprintf("mantzaris/%s.csv", y))
mine = load(sprintf("%s_jury.csv", y))
diff <- mine[colnames(mantz)] != mantz
which(diff, arr.ind = TRUE)
# Australia col 22 is not correct and Russia col 22 is not correct. We check the errors by hand
# on https://eurovision.tv/event/tel-aviv-2019/grand-final/results/ and see that the scraped 
# data is correct.
rm(diff)

# Check 2021:
# The dimensions are not equal, so we transpose mantz and drop the 'X' column
y = 2021
mine = load(sprintf("%s_jury.csv", y))
mantz = load(sprintf("mantzaris/%s.csv", y))
mantz = data.frame(t(subset(mantz, select= -c(X))))
all(mine == mantz[colnames(mine)])
# Now they are equal!

rm(mantz)
rm(mine)
rm(y)


