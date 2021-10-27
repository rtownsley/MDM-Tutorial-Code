#######################################################################################
##     computes third order, non stratified, age specific transition probabilities   ##  
##     for screening behavior given a window of observations accross a population    ##
##                                                                                   ##
##        Sample dataset available in repository                                     ##
##                      Written by:Rachel Townsley rachelmtownsley@gmail.com         ##
#######################################################################################

screenData<-select(screenData, scramID, dob, y2010, y2011, y2012, y2013, y2014)
#reshape data into long form
screenData_long<-melt(screenData, id=c("scramID", "dob"), variable.name = "year", value.name = "screen1")
screenData_long<-screenData_long[with(screenData_long,order(scramID, year)),]
#compute age based on screening year and DOB
screenData_long$age<-as.numeric(substr(screenData_long$year, 2,5))-screenData_long$dob+1

#drop DOB, no longer needed
screenData_long<-select(screenData_long, scramID, age, screen1)
#add column containing the next observed screening state for each ID 
screenData_long<-screenData_long%>%
  group_by(scramID) %>%
  mutate(screen2=dplyr::lag(screen1, n = 1, default = NA)) %>%
  mutate(screen3=dplyr::lag(screen2, n = 1, default = NA)) %>%
  mutate(nextScreen=dplyr::lead(screen1, n = 1, default = NA)) 

#delete rows where there is no "next screening" observed (ie the last year of observations)
screenData_long<-subset(screenData_long, (!is.na(nextScreen)) & (!is.na(screen3)))
screenData_long$fromState<-paste(screenData_long$screen3,screenData_long$screen2,screenData_long$screen1, sep="")
screenData_long<-select(screenData_long, age, fromState, nextScreen)
#generate transition frequency matrices for each age 
tbl<-dcast(as.data.frame(xtabs(~age+fromState+nextScreen, data=screenData_long)), age+fromState~nextScreen)
#compute transition probability matrices
probs<-cbind(tbl[,1:2],prop.table(as.matrix(tbl[,3:5]),1))
