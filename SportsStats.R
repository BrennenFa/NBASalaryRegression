install.packages('rvest')
install.packages('dplyr')
install.packages('ggplot2')
install.packages("randomForest")

library('rvest')
library('dplyr')
library('ggplot2')
library(randomForest)

#function --- get rid of dollar signs/commas in money
moneyFunction <- function(data, column_name) {
  data[[column_name]] <- gsub("\\$", "", data[[column_name]])
  data[[column_name]] <- gsub("\\,", "", data[[column_name]])
  data[[column_name]] <- as.numeric(data[[column_name]])
  
  return(data)
}


#create per game table
perGame <- "https://www.basketball-reference.com/leagues/NBA_2024_per_game.html"
perGameTable <- perGame %>%
  read_html() %>%
  html_element("table[id='per_game_stats']") %>%
  html_table()

#cleaning the data
perGameTable <- perGameTable[!(perGameTable$Rk=="Rk"), ]
perGameTable <- mutate(perGameTable, Age = as.numeric(Age))



#Create salary table
salary <- "https://www.basketball-reference.com/contracts/players.html"
salaryPage <- read_html(salary)
salaryElement <- html_element(salaryPage, "table[id='player-contracts']")
salaryTable <- html_table(salaryElement)

#change the column names
newNames <- salaryTable[1,]
salaryTable <- salaryTable[-c(1), ]
salaryTable <- salaryTable[-c(nrow(salaryTable)), ]
colnames(salaryTable) <- newNames
salaryTable <- salaryTable[!(salaryTable$Rk=="Rk"), ]
salaryTable <- salaryTable[!(salaryTable$`2023-24`=="Salary"), ]


#narrow down/clean the table
salaryTable <- rename(salaryTable, "Current Year Salary"="2023-24")
salaryTable <- select(salaryTable, Player, Tm, `Current Year Salary`, Guaranteed)
print(names(salaryTable))
salaryTable <- moneyFunction(salaryTable, "Guaranteed")
salaryTable <- moneyFunction(salaryTable, "Current Year Salary")


leagueTable <- inner_join(perGameTable, salaryTable, by = c("Player", "Tm"))
plotMP <- ggplot(data=leagueTable, aes(x=MP, y=`Current Year Salary`)) + geom_point()+geom_smooth()
plotPTS <- ggplot(data=leagueTable, aes(x=PTS, y=`Current Year Salary`)) + geom_point()+geom_smooth()
plotAGE <- ggplot(data=leagueTable, aes(x=Age, y=`Current Year Salary`)) + geom_point()+geom_smooth()

plotMP
plotPTS
plotAGE

#machine learning model
salaryPredictor <- randomForest(`Current Year Salary` ~ Age + MP + PTS + AST + TRB,
                                data= leagueTable,
                                ntree=201)      
salaryPredictor
plot(salaryPredictor)

