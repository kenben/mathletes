# This is an example of how to connect to the MySql database using python
# 
# For the MySQLdb library to work, I needed to install Python using the Microsoft
# MSI installer.  And then install MySQLdb also using its MSI installer.
# 
import MySQLdb

# connect
db = MySQLdb.connect(host = "mathletes2015.cxakoyfdtshs.eu-west-1.rds.amazonaws.com",
                    user="reader",
                    passwd="lesson_prep",
                    db="mathletes_anon")

cursor = db.cursor()

# execute SQL select statement
cursor.execute("SELECT * FROM county_province")

# commit your changes
db.commit()

# get the number of rows in the result set
numrows = int(cursor.rowcount)

# get and display one row at a time.
for x in range(0,numrows):
    row = cursor.fetchone()
    print row[0], "-->", row[1]
