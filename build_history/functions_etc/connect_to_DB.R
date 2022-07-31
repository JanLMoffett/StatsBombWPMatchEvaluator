
library(RODBC)

dbb <- "C:/Users/Jan/Documents/Projects/GithubRepositories/StatsBombHackathon/big_data/dataBombBay.accdb"
con <- odbcConnectAccess2007(dbb)

sqlTables(con)

query1 <- "SELECT * FROM Dbb_matches"

matches <- sqlQuery(con, query1)


odbcCloseAll()
