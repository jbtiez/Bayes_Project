#creating a blank dataframe
setwd('C:/Users/Jason/downloads')
alb <- read.csv('mtt.csv')

ed_data <- data.frame(matrix(NA,6114,8)) #creating a blank dataframe to put our student data
names(ed_data) <- c("School", "Year", "Race", "Gender", "Disadvantaged", "ESOL", "Disabled","Passed" )

#Doing the binary variables
start = 1
for(i in 1:170){ #running the for loop for each row
  N = alb[i,17] #getting the total number of students in each group
  end1 = start+N-1
  if(alb[i,8] == "M"){
    gender = "Male"
  } else {
    gender = "Female"
  }
  if(alb[i,10] == "N"){
    disadv = 0
  } else {
    disadv = 1
  }
  if(alb[i,11] == "N"){
    esl = 0
  } else {
    esl = 1
  }
  if(alb[i,12] == "N"){
    disab = 0
  } else {
    disab = 1
  }
    for(j in start:end1){
      ed_data[j,4] = gender
      ed_data[j,5] = disadv
      ed_data[j,6] = esl
      ed_data[j,7] = disab
    }
  start = end1 + 1
}


#putting in the exam results
start2 = 1
for(i in 1:170){
  NP = alb[i,16]
  end2 = start2+NP-1
  print(start2) #printing for diagnostic purposes (to see if function is running correctly)
  print(end2)
  for(j in start2:end2){
    ed_data[j,8] = 1
  }
  NF = alb[i,15] 
  if(NF != 0){ #if statement is necessary so counter doesn't go backwards when there are no fails
    start2 = end2 + 1
    end2 = start2 + NF - 1
    for(k in start2:end2){
      ed_data[k,8] = 0
    }
  }
  print(start2)
  print(end2)
  start2 = end2+1
}

#putting in the school
start3 = 1
for(i in 1:170){ #running the for loop for each row
  N = alb[i,17] #getting the total number of students in each group
  end3 = start3+N-1
  if(alb[i,5] == "Albemarle High"){
    school = "Albemarle"
  }
  else if(alb[i,5] == "Charlottesville High"){
    school = "Charlottesville"
  } 
  else if(alb[i,5] == "Monticello High"){
    school = "Monticello"
  } 
  else {
    school = "Western"}
  print(start3)
  print(end3)
  for(j in start3:end3){
    ed_data[j,1] = school
  }
  start3 = end3 + 1
}

#putting in the Race and Year
start4 = 1
for(i in 1:170){ #running the for loop for each row
  N = alb[i,17] #getting the total number of students in each group
  end4 = start4+N-1
  if(alb[i,7] == "Black, not of Hispanic origin"){
    race = "Black"
  }
  else if(alb[i,7] == "Hispanic"){
    race = "Hispanic"
  } 
  else if(alb[i,7] == "Non-Hispanic, two or more races"){
    race = "Other"
  }
  else if(alb[i,7] == "White, not of Hispanic origin"){
    race = "White"
  }
  if(alb[i,1] == "2009-2010"){
    year = "2010"
  }
  else if(alb[i,1] == "2010-2011"){
    year = "2011"
  } 
  else if(alb[i,1] == "2011-2012"){
    year = "2012"
  }
  else if(alb[i,1] == "2012-2013"){
    year = "2013"
  } else if(alb[i,1] == "2013-2014"){
  year = "2014"
  } else if(alb[i,1] == "2014-2015"){
  year = "2015"
  }
  else if(alb[i,1] == "2015-2016"){
    year = "2016"
  } 
  else if(alb[i,1] == "2016-2017"){
    year = "2017"
  }
  else if(alb[i,1] == "2017-2018"){
    year = "2018"
  } else if(alb[i,1] == "2018-2019"){
    year = "2019"
  }
  print(start4)
  print(end4)
  for(j in start4:end4){
    ed_data[j,3] = race
    ed_data[j,2] = year
  }
  start4 = end4 + 1
}

#checking the overall totals and confirming they match the original data
table(ed_data$Year)
table(ed_data$School)
table(ed_data$Race)
table(ed_data$Gender)
table(ed_data$Disadvantaged)
table(ed_data$ESOL)
table(ed_data$Disabled)
table(ed_data$Passed)


write.csv(ed_data, 'ed_data_final.csv')

#assign('ed_data',ed_data,envir=.GlobalEnv)

