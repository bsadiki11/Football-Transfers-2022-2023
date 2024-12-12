
library(dplyr)
library(ggplot2)

# Load CSV File
secondalltransfers <- read.csv("secondalltransfers.csv", stringsAsFactors = FALSE)

# Conversion Rate from Euro to USD.
exchange_rate <- 1.08

# Check the structure and unique values of the Transfer.Fee column.
str(secondalltransfers)
unique(secondalltransfers$Transfer.Fee)

# Remove Euro Sign, Remove M.
secondalltransfers$Transfer.Fee <- gsub("€", "", secondalltransfers$Transfer.Fee)   
secondalltransfers$Transfer.Fee <- gsub("m", "", secondalltransfers$Transfer.Fee)    
secondalltransfers$Transfer.Fee <- gsub(",", "", secondalltransfers$Transfer.Fee) 

# Only have Numbers and Decimals in the Transfer.Fee Column.
secondalltransfers$Transfer.Fee <- gsub("[^0-9.]", "", secondalltransfers$Transfer.Fee)

# Convert Transfer.Fee Column to numeric
secondalltransfers$Transfer.Fee <- as.numeric(secondalltransfers$Transfer.Fee)

# Convert Fees so that all values are in Millions
secondalltransfers <- secondalltransfers %>%
  mutate(
    Transfer.Fee = ifelse(Transfer.Fee < 1100.00 & Transfer.Fee > 125.00, Transfer.Fee /1000, Transfer.Fee)  # Convert to millions for values less than 1 million
  )

# Add the Exchange rate to the Transfer Fee Value.
secondalltransfers <- secondalltransfers %>% 
  mutate(Transfer.Fee = Transfer.Fee * exchange_rate)

# Edit Column Title's Name.
secondalltransfers <- secondalltransfers %>%
  rename(Joined.League = Joined.leauge)

# Make a Data Frame with only these 4 Columns.
FinalTF <- secondalltransfers %>%
  select(Player.Name, Player.Nationality, Joined.League, Transfer.Fee)

# Once Again Removes all "Loan" Transfers.
FinalTF <- FinalTF %>% 
  filter(Transfer.Fee !="Loan")


# New Data Frame revised. 
FinalTF_unique <- unique(FinalTF)

# Remove any repeats in names and values.
row.names(FinalTF_unique) <- NULL
nobs <- nrow(FinalTF_unique)
nseq <- seq(1:nobs[1])

# Have a sequence number for each Player
FinalTF_unique <- mutate(FinalTF_unique, nseq = nseq)

# Make Transfer.Fee Column Numeric
FinalTF_unique$Transfer.Fee <- as.numeric(FinalTF_unique$Transfer.Fee)

# Get the Total Cost of the Transfer Market - in millions (8,689 million = 8,689,093,560 = 8.6 Billion)
totaltransferfee <- sum(FinalTF_unique$Transfer.Fee, na.rm = TRUE)

# Total Number of Players we will observe from the Transfer Market
num_players <- nrow(FinalTF_unique)

# Calculate the average transfer fee across all players
average_transfer_fee <- totaltransferfee / num_players 


# PART II

# League Counts to show how many players joined each league
league_counts <- FinalTF_unique %>%
  group_by(Joined.League) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# Sets the top 5 Leagues ready for Pie Chart Display
top_leagues <- league_counts %>%
  top_n(5, count)  

# Create League Data Frames top 5 most viewed and known Leagues
PremierLeaguedf <- subset(FinalTF_unique, Joined.League == "Premier League")
Bundesligadf <- subset(FinalTF_unique, Joined.League == "Bundesliga")
LaLigadf <- subset(FinalTF_unique, Joined.League == "LaLiga")
SerieAdf <- subset(FinalTF_unique, Joined.League == "Serie A")
Ligue1df <- subset(FinalTF_unique, Joined.League == "Ligue 1")

# Number of players transferred to each League
numPL <- nrow(PremierLeaguedf)
numBL <- nrow(Bundesligadf)
numLL <- nrow(LaLigadf)
numSA <- nrow(SerieAdf)
numL1 <- nrow(Ligue1df)

# Total Cost by League in Millions across top 5 Leagues
PLTotalCost <- sum(PremierLeaguedf$Transfer.Fee, na.rm = TRUE)
BLTotalCost <- sum(Bundesligadf$Transfer.Fee, na.rm = TRUE)
LLTotalCost <- sum(LaLigadf$Transfer.Fee, na.rm = TRUE)
SATotalCost <- sum(SerieAdf$Transfer.Fee, na.rm = TRUE)
L1TotalCost <- sum(Ligue1df$Transfer.Fee, na.rm = TRUE)

# Average Cost by League
PLAVGCost <- PLTotalCost / numPL
BLAVGCost <- BLTotalCost / numBL
LLAVGCost <- LLTotalCost / numLL
SAAVGCost <- SATotalCost / numSA
L1AVGCost <- L1TotalCost / numL1



# PART III

library(ggplot2)

# Create Data Frame that has only the Leagues and Total Costs.
data <- data.frame(
  Leagues = c("Premier League", "Bundesliga", "LaLiga", "Serie A", "Ligue 1"),
  TotalCosts = c(3302.8452, 739.8000, 597.0888, 969.9480, 739.4220)
)

# Create Histogram to compare Total Cost amongst the top 5 Leagues. 
ggplot(data, aes(x = Leagues, y = TotalCosts, fill = Leagues)) +
  geom_bar(stat = "identity") +                   
  labs(
    title = "Total Transfer Costs by League",
    x = "Leagues",
    y = "Total Costs (in millions)"
  ) +
  theme_minimal() +              
  theme(legend.position = "none")  

# How many Counts of Each Nationality there are in a data frame.
nationality_counts <- FinalTF_unique %>%
  group_by(Player.Nationality) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Sets the top 5 Nationalities ready for Pie Chart Display
top_nationalities <- nationality_counts %>%
  top_n(5, count) 


# Create Data Frames for Each Nationality of Player
Argentinadf <- subset(FinalTF_unique, Player.Nationality == "Argentina")
Brazildf <- subset(FinalTF_unique, Player.Nationality == "Brazil")
Francedf <- subset(FinalTF_unique, Player.Nationality == "France")
Italydf <- subset(FinalTF_unique, Player.Nationality == "Italy")
Spaindf <- subset(FinalTF_unique, Player.Nationality == "Spain")


# Number of players from each nationality
numArg <- nrow(Argentinadf)
numBra <- nrow(Brazildf)
numFr <- nrow(Francedf)
numIta <- nrow(Italydf)
numSpa <- nrow(Spaindf)



# Total transfer sums of each nationality
ARGTotalCost <- sum(Argentinadf$Transfer.Fee, na.rm = TRUE)
BRATotalCost <- sum(Brazildf$Transfer.Fee, na.rm = TRUE)
FRATotalCost <- sum(Francedf$Transfer.Fee, na.rm = TRUE)
ITATotalCost <- sum(Italydf$Transfer.Fee, na.rm = TRUE)
SPATotalCost <- sum(Spaindf$Transfer.Fee, na.rm = TRUE)


# Average Cost per Nationality
ARGAVGCost <- ARGTotalCost / numArg
print(ARGAVGCost)
BRAAVGCost <- BRATotalCost / numBra
print(BRAAVGCost)
FRAAVGCost <- FRATotalCost / numFr
print(FRAAVGCost)
ITAAVGCost <- ITATotalCost / numIta
print(ITAAVGCost)
SPAAVGCost <- SPATotalCost / numSpa
print(SPAAVGCost)



# New data frame for the histogram
Nationality <- c("Argentina", "Brazil", "France", "Italy", "Spain")
TotalNCosts <- c(ARGTotalCost, BRATotalCost, FRATotalCost, ITATotalCost, SPATotalCost)
Nationdf <- data.frame(
  Nationality,
  TotalNCosts
)

#Histogram that shows Total Price per Nationality

ggplot(Nationdf, aes(x = Nationality, y = TotalNCosts, fill = Nationality)) +
  geom_bar(stat = "identity") +                   
  labs(
    title = "Total Transfer Costs by Nationality",
    x = "Nationality",
    y = "Total Costs (in millions)"
  ) +
  theme_minimal() +              
  theme(legend.position = "none") 



library(treemap)

# Create Data Frame to Make a Tree Map
Treemaptotaldf <- FinalTF_unique%>%
  filter(Joined.League %in% c("Premier League", "Bundesliga", "LaLiga", "Serie A", "Ligue 1")) %>% 
  group_by(Joined.League, Player.Nationality) %>% 
  summarise(totaltransferfee = sum(Transfer.Fee, na.rm = TRUE), .groups = "drop") %>% 
  ungroup()


# Tree map to show all the nationalities that were bought by each league. 
treemap(Treemaptotaldf, 
        index = c("Joined.League", "Player.Nationality"),
        vSize = "totaltransferfee",
        type = "index",
        border.col = c("white", "green", "red", "yellow"),
        border.lwds = c(3, 4, 2, 1),
        title = "League Treemap",
        frontsize.title = 14,
        frontsize.labels = c(14, 12, 10, 10),
        align.labels = c("center", "center", "right", "bottom")
)

# Check for NAs after conversion
if (any(is.na(secondalltransfers$Transfer.Fee))) {
  warning("There are NA values in Transfer.Fee after cleaning and conversion.")
}



library(dplyr)
library(ggplot2)


# Create the pie chart to show the Number of Nationalities out of all the Transfer Market. 
ggplot(top_nationalities, aes(x = "", y = count, fill = Player.Nationality)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  
  coord_polar(theta = "y") + 
  labs(title = "Distribution of Nationalities in Transfer Market") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste(count)), position = position_stack(vjust = 0.5), color = "black")  # Add count labels


# Create the pie chart to show the number of players that joined each different league. 
ggplot(top_leagues, aes(x = "", y = count, fill = Joined.League)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  
  coord_polar(theta = "y") +  
  labs(title = "Distribution of Players Across Leagues") +
  theme_void() +  
  theme(legend.position = "right") +  
  geom_text(aes(label = paste(count)), position = position_stack(vjust = 0.5), color = "black")







