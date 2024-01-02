install.packages('rvest')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('randomForest')
install.packages('writexl')


library('rvest')
library('dplyr')
library('ggplot2')
library('randomForest')
library('writexl')



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



#Wins and Losses
recordLink <- "https://www.covers.com/sport/basketball/nba/standings/2022-2023"
recordTables <- recordLink %>%
  read_html() %>%
  html_elements("table[class='table covers-CoversMatchups-Table covers-CoversStandings-table']")


#combine tables
recordTable1 <- html_table(recordTables[[1]])
recordTable2 <- html_table(recordTables[[2]])



recordTable1 <- rename(recordTable1, Tm=Atlantic)
recordTable2 <- rename(recordTable2, Tm=Northwest)

recordTable <- full_join(recordTable1, recordTable2, by=c("Tm", "PCT"))
recordTable <- select(recordTable, "Tm", "PCT")


recordTable["Tm"][recordTable["Tm"] == "CHA"] <- "CHO"
recordTable["Tm"][recordTable["Tm"] == "BK"] <- "BRK"
recordTable["Tm"][recordTable["Tm"] == "GS"] <- "GSW"
recordTable["Tm"][recordTable["Tm"] == "NO"] <- "NOP"
recordTable["Tm"][recordTable["Tm"] == "NY"] <- "NYK"
recordTable["Tm"][recordTable["Tm"] == "SA"] <- "SAS"







#Data Plot Work
leagueTable <- inner_join(perGameTable, salaryTable, by = c("Player", "Tm"))
leagueTable <- full_join(leagueTable, recordTable, by = c("Tm"))
leagueTable <- leagueTable[!is.na(leagueTable$Player),]

#changing factors to numeric
names <- select(leagueTable, Player)
leagueTable <- select(leagueTable, `Current Year Salary`, Age, MP, PTS, AST, TRB, TOV, PCT)
leagueTable <- sapply(leagueTable, as.numeric)
leagueTable <- as.data.frame(leagueTable)
leagueTable <- cbind(names, leagueTable)





plotMP <- ggplot(data=leagueTable, aes(x=MP, y=`Current Year Salary`)) + geom_point()+geom_smooth()
plotPTS <- ggplot(data=leagueTable, aes(x=PTS, y=`Current Year Salary`)) + geom_point()+geom_smooth()
plotAGE <- ggplot(data=leagueTable, aes(x=Age, y=`Current Year Salary`)) + geom_point()+geom_smooth()

plotMP
plotPTS
plotAGE

#organizing training and testing data
sampleSize <- floor(0.75 * nrow(leagueTable))
set.seed(12)
train_ind <- sample(seq_len(nrow(leagueTable)), size = sampleSize)

training <- leagueTable[train_ind, ]
testing <- leagueTable[-train_ind, ]


#machine learning model
salaryPredictor <- randomForest(`Current Year Salary` ~ Age + MP + PTS + AST + TRB + TOV + PCT,
                                data= training,
                                ntree=201)      
salaryPredictor
plot(salaryPredictor)

predictions <- predict(salaryPredictor, newdata = testing)

mse <- mean((testing$`Current Year Salary` - predictions)^2)
rmse <- sqrt(mse)  
r_squared <- 1 - (sum((testing$`Current Year Salary` - predictions)^2) / sum((testing$`Current Year Salary` - mean(testing$`Current Year Salary`))^2))

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")

`Predicted Salary` <- predict(salaryPredictor, newdata = leagueTable)
Residual <- leagueTable$`Current Year Salary` - `Predicted Salary`

residualTable <- cbind(leagueTable, `Predicted Salary`, Residual)
residualTable <- select(residualTable, Player, `Current Year Salary`, `Predicted Salary`, Residual)

write_xlsx(residualTable, "residualTable.xlsx")





