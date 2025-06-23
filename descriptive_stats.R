library(ggplot2)
library(plyr)
`%>%` <- dplyr::`%>%`

source("utils.R")

dfs = load_all()

################################################################################## 
###### Visualization 1: Average percentage of total points achieved in each ######
###### year when a country participated                                     ######
##################################################################################

# Normalize dfs and divide each year by the total amount of points received
percentage_dfs = lapply(dfs, function(x) as.data.frame(x / sum(x)))
all_countries = get_countryNames(dfs)

all_points = lapply(percentage_dfs, colSums)
names(all_points) = c()

# Count number of times a country participated in the finals
country_cnts = plyr::count(names(unlist(all_points)))
rownames(country_cnts) = country_cnts$x

# Sum together all values for the countries 
agg_points = as.data.frame(rowsum(unlist(all_points), names(unlist(all_points)))) %>%
  merge(y=country_cnts, by.x="row.names", by.y="x") %>%
  rename(c("Row.names"="country", "V1"="sum"))
agg_points$avg = agg_points$sum / agg_points$freq
agg_points$country = as.character(agg_points$country)

# Remove participants with freq <= 5 and sort
agg_points = agg_points[agg_points$freq > 5,] %>%
  arrange(desc(avg))

mean_pts = mean(agg_points$avg)
sd_pts = sd(agg_points$avg)

ggplot(data=rbind(head(agg_points, 10), tail(agg_points, 10)), 
       aes(x=avg, y=reorder(country, -avg))) + 
  geom_col(fill=c(rep("green3",10), rep("red",10))) +
  geom_vline(xintercept=mean_pts) +
  geom_vline(xintercept=c(mean_pts-sd_pts, mean_pts+sd_pts), linetype="dashed") +
  labs(x="Average percentage of total points",
       y="") 

################################################################################## 
###### Visualization 2: Mean points given to a country by another country   ######
##################################################################################
get_meanVote_and_count = function(country1, country2, dfs) {
  votes = get_givenVotes(country1, country2, dfs, 1975, 2023)
  count = length(votes[!is.na(votes)])
  mean = if (count == 0) 0 else mean(votes, na.rm=TRUE)
  ret = c(mean, count)
  names(ret) = c("mean", "count")
  return(ret)
}

countryPairs = expand.grid(Giver = all_countries, Receiver = all_countries, stringsAsFactors=FALSE)
res = mapply(get_meanVote_and_count, 
             countryPairs$Giver, countryPairs$Receiver,
             MoreArgs = list(dfs=dfs),
             USE.NAMES = FALSE)
countryPairs$meanVote = res["mean",]
countryPairs$count = res["count",]
rm(res)

# Get mean and standard deviation from the data to plot in histogram
dd = countryPairs[countryPairs$count>0,"meanVote"]
stats = data.frame(Statistic = c("mean", "mean+sd", "mean+2sd", "mean+3sd"),
                   val = c(mean(dd),
                           mean(dd) + sd(dd),
                           mean(dd) + 2*sd(dd),
                           mean(dd) + 3*sd(dd)))
rm(dd)

# Visualize meanVotes in a Histogram
ggplot(countryPairs[countryPairs$count >= 5,], aes(x=meanVote)) + 
  geom_histogram(color="black", fill="grey") + 
  xlab("Mean Vote") +
  ylab(NULL) +
  geom_vline(data=stats, aes(xintercept = val,
                            linetype = Statistic,
                            col = Statistic), linewidth=1)
  
# Filter out the highest means for countries with more than 5 shared participations
highMeans = countryPairs[countryPairs$count >= 5 & countryPairs$meanVote > 10, ] %>% 
  arrange(meanVote)

# Construct a voting matrix containing a list of all the votes given by a country
# to another one over all years
votingMatrix = mapply(get_givenVotes, highMeans$Giver, highMeans$Receiver, 
                       MoreArgs=list(dfs=dfs, startYear=1975, endYear=2023),
                       USE.NAMES = FALSE)
colnames(votingMatrix) = paste(highMeans$Giver, highMeans$Receiver, sep=" => ")
votingMatrix = as.data.frame(votingMatrix)

# Boxplot visualizes these distributions
ggplot(stack(votingMatrix), aes(x=ind, y=values)) +
  geom_boxplot(na.rm=TRUE) + 
  stat_summary(fun=mean, geom="point", shape="|", size=5, color="red", na.rm=TRUE) +
  xlab(NULL) +
  ylab("Points") +
  coord_flip() +
  theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))


################################################################################## 
###### Visualization 3: Countries that gave 8 - 12 points to each other     ###### 
###### in the same year                                                     ######   
################################################################################## 
collusion_over_years = function(country1, country2, dfs, startYear, endYear) {
  check_vote = function(v1, v2){
    if (is.na(v1) | is.na(v2)) {
      return(FALSE)
    }
    if (v1 >= 8 & v2 >= 8) {
      return(TRUE)
    }
    return(FALSE)
  }
  
  v_c1 = get_givenVotes(country1, country2, dfs, startYear, endYear)
  v_c2 = get_givenVotes(country2, country1, dfs, startYear, endYear)
  
  cnt = sum(mapply(check_vote, v_c1, v_c2))
  return(cnt)
}

countryPairs$numCollusions = mapply(collusion_over_years, 
                                    countryPairs$Giver, countryPairs$Receiver, 
                                    MoreArgs = list(dfs=dfs, startYear=1975, endYear=2023),
                                    USE.NAMES = FALSE)

ggplot(countryPairs[countryPairs$count >= 5,], aes(x=numCollusions)) + 
  geom_histogram(color="black", fill="grey") +
  ylab(NULL) 

colluded = countryPairs[countryPairs$numCollusions >= 5,]

# Remove the pair duplicates
colluded$pairName = mapply(get_standardizedPairName,
                           colluded$Giver, colluded$Receiver)
colluded = colluded[duplicated(colluded$pairName),]

ggplot(data=colluded, aes(x=numCollusions, y=reorder(pairName, numCollusions))) + 
  geom_col() +
  labs(x="times where >= 8 points were given",
       y="") +
  theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
