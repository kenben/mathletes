# Mathletes Challange Sample R script to access the MySQL Database

# First install the RMYSQL pachage from the CRAN repository

# Then load the pachage
library(RMySQL)

# The following line connects to the database.  Fill in the required password.
con <- dbConnect(RMySQL::MySQL(), host = "mathletes2015.cxakoyfdtshs.eu-west-1.rds.amazonaws.com", 
                 dbname = "mathletes_anon", user = "reader", password = "lesson_prep")


# Some basic info functions
summary(con)
dbGetInfo(con)
dbListResults(con)

# Get a list of the tables and write to a csv file
list.of.tables <- dbListTables(con)
write.csv(list.of.tables, file = "list_of_tables.csv")

#res <- dbSendQuery(con, "SELECT * FROM students_energy_points LIMIT 1000, 1000")
res <- dbSendQuery(con, "SELECT * FROM students_energy_points LIMIT 0, 1000")
fetch <- dbFetch(res)
dbClearResult(res)

# Example of reading a table into a dataframe
county.province <- dbGetQuery(con, "SELECT * FROM county_province")

# Example work on the sudent energy points
students.energy.points <- dbGetQuery(con, "SELECT * FROM students_energy_points")
summary(students.energy.points)
hist(students.energy.points$energy_points)

# The progress.changes table is almost 500MB so limit the read to avoid delays
progress.changes <- dbGetQuery(con, "SELECT * FROM progress_changes LIMIT 0, 1000")
write.csv(progress.changes, file = "progress_changes.csv")


# Remember to disconnect from the database
dbDisconnect(con)
