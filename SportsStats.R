install.packages('rvest')
install.packages("dplyr")

library('rvest')
library('dplyr')

#create per game table
perGame <- "https://www.basketball-reference.com/teams/ORL/2024.html"
perGameTable <- perGame %>%
  read_html() %>%
  html_element("table[id='per_game']") %>%
  html_table()

str(perGameTable)



#Create salary table
salary <- "https://www.basketball-reference.com/contracts/ORL.html"
salaryPage <- read_html(salary)
salaryElement <- html_element(salaryPage, "table[id='contracts']")
salaryTable <- html_table(salaryElement)

#change the column names
newNames <- salaryTable[1,]
salaryTable <- salaryTable[-c(1), ]
salaryTable <- salaryTable[-c(nrow(salaryTable)), ]
colnames(salaryTable) <- newNames

#narrow down/clean the table
salaryTable <- select(salaryTable, Player, Age, Guaranteed)
salaryTable <- salaryTable[!(is.na(salaryTable$Guaranteed) | salaryTable$Guaranteed==""),]
salaryTable <- mutate(salaryTable, Guaranteed=gsub('\\$','',Guaranteed))
salaryTable <- mutate(salaryTable, Guaranteed=gsub('\\,','',Guaranteed))
salaryTable <- mutate(salaryTable, Age = as.numeric(Age))
salaryTable <- mutate(salaryTable, Guaranteed = as.numeric(Guaranteed))
str(salaryTable)



magicTable <- inner_join(perGameTable, salaryTable, by = c("Player", "Age"))
print(magicTable)

