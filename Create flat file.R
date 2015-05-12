library(RMySQL)
# The following line connects to the database.  Fill in the required password.
con <- dbConnect(RMySQL::MySQL(), host = "mathletes2015.cxakoyfdtshs.eu-west-1.rds.amazonaws.com", 
                 dbname = "mathletes_anon", user = "reader", password = "?????????")

progress_changes <- dbGetQuery(con, "SELECT * FROM progress_changes")
#Alternatively load in smaller files if having trouble running the full file, for example:
progress_changes <- dbGetQuery(con, "SELECT * FROM progress_changes where SUBSTRING(user_id,8,8)='facebook'")	#60 users
progress_changes <- dbGetQuery(con, "SELECT * FROM progress_changes where SUBSTRING(user_id,8,8)='googleid'")	#1072 users
progress_changes <- dbGetQuery(con, "SELECT * FROM progress_changes where SUBSTRING(user_id,8,8)='nouserid'")	#10 users

#add field "period" which will indicate the time period as prior to 2015 competition start date (0) or by week during the competition
progress_changes$period <-ifelse(as.Date(progress_changes$date)<as.Date("2015-01-05"),0,progress_changes$dateWeek)
#Establish what level was achieved on each activity
progress_changes$practiced <-ifelse(progress_changes$to_progress_level=="practiced",1,0)
progress_changes$mastery1 <-ifelse(progress_changes$to_progress_level=="mastery1",1,0)
progress_changes$mastery2 <-ifelse(progress_changes$to_progress_level=="mastery2",1,0)
progress_changes$mastery3 <-ifelse(progress_changes$to_progress_level=="mastery3",1,0)

#Group by user_id, exercise_name and period (week) - this condenses all activity per user per week on a particular activity to the highest mastery level achieved
progress_changes <-aggregate(cbind(practiced, mastery1, mastery2, mastery3)~user_id+exercise_name+period, data=progress_changes, FUN=sum)
progress_changes$score <-ifelse(progress_changes$mastery3>0,3,ifelse(progress_changes$mastery2>0,2.5,ifelse(progress_changes$mastery1>0,1.5,ifelse(progress_changes$practiced>0,1,0))))

#Calculate cumulative progress on each activity per user at the end of each week
progress_changes$week0 <-ifelse(progress_changes$period==0, progress_changes$score, 0)
progress_changes$week1 <-ifelse(progress_changes$period==1, progress_changes$score, progress_changes$week0)
progress_changes$week2 <-ifelse(progress_changes$period==2, progress_changes$score, progress_changes$week1)
progress_changes$week3 <-ifelse(progress_changes$period==3, progress_changes$score, progress_changes$week2)
progress_changes$week4 <-ifelse(progress_changes$period==4, progress_changes$score, progress_changes$week3)
progress_changes$week5 <-ifelse(progress_changes$period==5, progress_changes$score, progress_changes$week4)
progress_changes$week6 <-ifelse(progress_changes$period==6, progress_changes$score, progress_changes$week5)
progress_changes$week7 <-ifelse(progress_changes$period==7, progress_changes$score, progress_changes$week6)
progress_changes$week8 <-ifelse(progress_changes$period==8, progress_changes$score, progress_changes$week7)
progress_changes$week9 <-ifelse(progress_changes$period==9, progress_changes$score, progress_changes$week8)
progress_changes$week10 <-ifelse(progress_changes$period==10, progress_changes$score, progress_changes$week9)
progress_changes$week11 <-ifelse(progress_changes$period==11, progress_changes$score, progress_changes$week10)
progress_changes$week12 <-ifelse(progress_changes$period==12, progress_changes$score, progress_changes$week11)

#Now we group by user_id and exercise_name.  The period (week) has been set out as columns so we no longer need it as rows.
progress_changes <-aggregate(cbind(week0, week1, week2, week3, week4, week5, week6, week7, week8, week9, week10, week11, week12)~user_id+exercise_name, data=progress_changes, FUN=max)
#We now have one row for each user and exercise, tracking progress for that user on that exercise across the weeks.

#merge curriculum info onto progress_changes
curriculum <-read.csv("curriculum.csv")
progress_changes <-merge(progress_changes, curriculum, by.x = "exercise_name", by.y = "exercise_name", all.x=TRUE)
progress_changes$wtd0 <-progress_changes$week0 * progress_changes$weight
progress_changes$wtd12 <-progress_changes$week12 * progress_changes$weight

#compare points to "points_in_comp" to validate
points_in_comp <- dbGetQuery(con, "SELECT * FROM points_in_comp")
points_check <-aggregate(cbind(week0, week12, wtd0, wtd12)~user_id, data=progress_changes, sum)
points_check<-merge(points_check, points_in_comp, by.x = "user_id", by.y = "user_id", all.x=TRUE)
points_check$diffold <-points_check$week0 - points_check$points_earned_pre_comp
points_check$diffnew <-points_check$week12-points_check$week0 - points_check$points_earned_in_comp
write.table(points_check, file="points_check.txt", sep="\t")
#points_in_comp has 400 extra points, most likely due to a later cut-off.  Otherwise the points_in_comp matches the unweighted scores.

#Calculate incremental points earned each week
progress_changes$points_week0 <-progress_changes$week0
progress_changes$points_week1 <-progress_changes$week1 - progress_changes$week0 
progress_changes$points_week2 <-progress_changes$week2 - progress_changes$week1
progress_changes$points_week3 <-progress_changes$week3 - progress_changes$week2
progress_changes$points_week4 <-progress_changes$week4 - progress_changes$week3
progress_changes$points_week5 <-progress_changes$week5 - progress_changes$week4
progress_changes$points_week6 <-progress_changes$week6 - progress_changes$week5
progress_changes$points_week7 <-progress_changes$week7 - progress_changes$week6
progress_changes$points_week8 <-progress_changes$week8 - progress_changes$week7
progress_changes$points_week9 <-progress_changes$week9 - progress_changes$week8
progress_changes$points_week10 <-progress_changes$week10 - progress_changes$week9
progress_changes$points_week11 <-progress_changes$week11 - progress_changes$week10
progress_changes$points_week12 <-progress_changes$week12 - progress_changes$week11

#Add columns to track level of difficulty of exercises by week
progress_changes$mission <- as.numeric(as.character(progress_changes$mission_base_year)) #note that nonmath becomes NA.
progress_changes$week0_mission <-progress_changes$points_week0 * progress_changes$mission
progress_changes$week1_mission <-progress_changes$points_week1 * progress_changes$mission
progress_changes$week2_mission <-progress_changes$points_week2 * progress_changes$mission
progress_changes$week3_mission <-progress_changes$points_week3 * progress_changes$mission
progress_changes$week4_mission <-progress_changes$points_week4 * progress_changes$mission
progress_changes$week5_mission <-progress_changes$points_week5 * progress_changes$mission
progress_changes$week6_mission <-progress_changes$points_week6 * progress_changes$mission
progress_changes$week7_mission <-progress_changes$points_week7 * progress_changes$mission
progress_changes$week8_mission <-progress_changes$points_week8 * progress_changes$mission
progress_changes$week9_mission <-progress_changes$points_week9 * progress_changes$mission
progress_changes$week10_mission <-progress_changes$points_week10 * progress_changes$mission
progress_changes$week11_mission <-progress_changes$points_week11 * progress_changes$mission
progress_changes$week12_mission <-progress_changes$points_week12* progress_changes$mission

#aggregate across exercises to create a flatfile where each user appears once
flatfile <-aggregate(cbind(points_week0, points_week1, points_week2, points_week3, points_week4, points_week5, points_week6, points_week7, points_week8, points_week9, points_week10, points_week11, points_week12, week0_mission, week1_mission, week2_mission, week3_mission, week4_mission, week5_mission, week6_mission, week7_mission, week8_mission, week9_mission, week10_mission, week11_mission, week12_mission)~user_id, data=progress_changes, sum)

#The mission level now needs to be adjusted as the grouping used a sum function and we need the average mission level for each week.
flatfile$week0_mission <-ifelse(flatfile$points_week0==0,0,flatfile$week0_mission/flatfile$points_week0)
flatfile$week1_mission <-ifelse(flatfile$points_week1==0,0,flatfile$week1_mission/flatfile$points_week1)
flatfile$week2_mission <-ifelse(flatfile$points_week2==0,0,flatfile$week2_mission/flatfile$points_week2)
flatfile$week3_mission <-ifelse(flatfile$points_week3==0,0,flatfile$week3_mission/flatfile$points_week3)
flatfile$week4_mission <-ifelse(flatfile$points_week4==0,0,flatfile$week4_mission/flatfile$points_week4)
flatfile$week5_mission <-ifelse(flatfile$points_week5==0,0,flatfile$week5_mission/flatfile$points_week5)
flatfile$week6_mission <-ifelse(flatfile$points_week6==0,0,flatfile$week6_mission/flatfile$points_week6)
flatfile$week7_mission <-ifelse(flatfile$points_week7==0,0,flatfile$week7_mission/flatfile$points_week7)
flatfile$week8_mission <-ifelse(flatfile$points_week8==0,0,flatfile$week8_mission/flatfile$points_week8)
flatfile$week9_mission <-ifelse(flatfile$points_week9==0,0,flatfile$week9_mission/flatfile$points_week9)
flatfile$week10_mission <-ifelse(flatfile$points_week10==0,0,flatfile$week10_mission/flatfile$points_week10)
flatfile$week11_mission <-ifelse(flatfile$points_week11==0,0,flatfile$week11_mission/flatfile$points_week11)
flatfile$week12_mission <-ifelse(flatfile$points_week12==0,0,flatfile$week12_mission/flatfile$points_week12)

#merge columns to indicate the students class (grade level) and school
student_class_list <- dbGetQuery(con, "SELECT * FROM student_class_list")	#16876 rows
class_sort <-student_class_list[order(-student_class_list$mathletes_class, na.last=FALSE) , ]	#16876 rows
class_list <- class_sort[!duplicated(class_sort$student_user_id), ] 	#13486 rows (duplicates removed)
#DATA CLEANING
class_list$class_year[class_list$class_name=="3rdmathletes" & class_list$mathletes_class==0] <-"3rd class (ROI)/Primary 5 (NI)"
class_list$class_year[class_list$class_name=="5th class mathletes" & class_list$mathletes_class==0] <-"5th class (ROI)/Primary 7 (NI)"
class_list$class_year[class_list$class_name=="fifthclassmathletes" & class_list$mathletes_class==0] <-"5th class (ROI)/Primary 7 (NI)"
class_list$class_year[class_list$class_name=="6th class mathletes" & class_list$mathletes_class==0] <-"6th class (ROI)/Year 8 (NI)"
class_list$class_year[class_list$class_name=="6th ClassMathletes" & class_list$mathletes_class==0] <-"6th class (ROI)/Year 8 (NI)"
class_list$class_year[class_list$class_name=="6thmathletes" & class_list$mathletes_class==0] <-"6th class (ROI)/Year 8 (NI)"
class_list$class_year[class_list$class_name=="istyearmathletes" & class_list$mathletes_class==0] <-"1st year (ROI)/Year 9 (NI)"
class_list$class_year[class_list$class_name=="1031styearmathletes" & class_list$mathletes_class==0] <-"1st year (ROI)/Year 9 (NI)"
class_list$class_year[class_list$class_name=="year9mathletes" & class_list$mathletes_class==0] <-"1st year (ROI)/Year 9 (NI)"
class_list$class_year[class_list$class_name=="2nd Higher Level Maths" & class_list$mathletes_class==0] <-"2nd year (ROI)/Year 10 (NI)"
class_list$class_year[class_list$class_name=="2ndmathletes" & class_list$mathletes_class==0] <-"2nd year (ROI)/Year 10 (NI)"
class_list$class_year[class_list$class_name=="year10mathletes" & class_list$mathletes_class==0] <-"2nd year (ROI)/Year 10 (NI)"
class_list$class_year[class_list$class_name=="ThirdyearMathletes" & class_list$mathletes_class==0] <-"3rd year (ROI)/Year 11 (NI)"
class_list$class_year[class_list$class_name=="TY1" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="ty1mathletes" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="TY4Amathletes" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="TY34thyearmathletes" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="6thyearmathletes" & class_list$mathletes_class==0] <-"5th & 6th year (ROI)/Year 14 (NI)"
class_list <-class_list[ , c(2,4)]	#keep only the student_user_id and class_year
flatfile <-merge(flatfile, class_list, by.x="user_id", by.y="student_user_id", all.x=TRUE)

student_school_list <- dbGetQuery(con, "SELECT student_user_id, school_roll_number, school_county FROM student_school_list")
student_school_list <- dbGetQuery(con, "SELECT * FROM student_school_list")	#13490 rows
school_sort <-student_school_list[order(student_school_list$school_roll_number, student_school_list$school_county, na.last=TRUE) , ]	#13490 rows
school_list <- school_sort[!duplicated(school_sort$student_user_id), ] 	#13486 rows
school_list <-school_list[ , c(2,4,5,6)] #keep only student_user_id, school_roll_number, school_town, school_county
flatfile <-merge(flatfile, school_list, by.x="user_id", by.y="student_user_id", all.x=TRUE)

#calculate total points and bonus points per student
flatfile$points_in_comp <-flatfile$points_week1+flatfile$points_week2+flatfile$points_week3+flatfile$points_week4+flatfile$points_week5+flatfile$points_week6+flatfile$points_week7+flatfile$points_week8+flatfile$points_week9+flatfile$points_week10+flatfile$points_week11+flatfile$points_week12
flatfile$champions_league <-ifelse(flatfile$points_week0>250,1,0)
flatfile$bonus_points <- flatfile$champions_league * flatfile$points_in_comp * flatfile$points_week0/2877
flatfile$total_points <- flatfile$points_in_comp + flatfile$bonus_points
write.table(flatfile, file="flatfile.txt", sep="\t")

#aggregate by Irish class level to examine broad trends across the duration of the competition
class_stats <-flatfile[,c(28, 2:27,31:33)]
trend_by_class <-aggregate(class_stats[,-1], by=list(class_stats$class_year), FUN=mean, na.rm=TRUE)
#A few plots for a quick look at trends by class
irish_classes <-trend_by_class[,1]
cols <-c("red","green","orange","blue","yellow","black","pink","brown")

##############
heading="average points earned per week"
x <-c(0:12)  #horizontal axis will show the week
y <-trend_by_class[1,c(2:14)]
plot(x,y,type="n", main=heading)
for (i in 1: length(irish_classes)){
	y <-trend_by_class[i,c(2:14)]	#average points earned per week
	par(pch=22, col=cols[i]) #plotting symbol and colour
	lines(x,y, type="l")
	}
##############
heading="average difficulty level per week"	
x <-c(0:12)  #horizontal axis will show the week
y <-trend_by_class[3,c(15:27)]
plot(x,y,type="n", main=heading)
for (i in 1:length(irish_classes)){
	y <-trend_by_class[i,c(15:27)]		
	par(pch=22, col=cols[i]) #plotting symbol and colour
	lines(x,y, type="l")
	}
##############


# Remember to disconnect from the database
dbDisconnect(con)
