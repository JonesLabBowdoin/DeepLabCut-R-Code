#Get all excel sheets into their own dataframes
library(readr)
library(stringr)
library(dplyr)
library(tidyr)

setwd("/Volumes/hpc-research/pjones3/rwarburt/Summer2021-rwarburt-2022-01-02/labeled-data")

#Get list of all videos 
dirs<-list.dirs(path =".", full.names = TRUE)
dirs<-as.list(dirs)
dirs[[1]] = NULL

#Get all labeled data into one dataframe
c<- data.frame(bodyparts= c(1),
               coords= c(1),
               labeleddata= c(1),
               bee= c(1))
setwd("/Volumes/hpc-research/pjones3/rwarburt/Summer2021-rwarburt-2022-01-02/labeled-data")
for(i in 1:192) {
  setwd(paste(dirs[i]))
  a<-read_csv("CollectedData_rwarburt.csv")
  b <- data.frame(t(a[-1]))
  colnames(b) <- c("bodyparts",'coords',"labeleddata")
  rownames(b)<-NULL
  b$bee<-paste(dirs[[i]])
  c<-rbind(c,b)
  
  setwd("/Volumes/hpc-research/pjones3/rwarburt/Summer2021-rwarburt-2022-01-02/labeled-data")
}


#get rid of ./ in the list of videos and in c!
dirs<-substring(dirs, 3)
dirs<-as.list(dirs)
c$bee<-substring(c$bee, 3)

#Clean up data and add ID column
c2<-c
c2<-c2[-1,]

#c2$bee<-substring(c2$bee, 3)
c2$ID<-paste(c2$bee, c2$bodyparts,c2$coords)
c2$ID<-str_remove_all(c2$ID, " ")



#Turn all csv outputs into good outputs with names that match dirs list
setwd("/Volumes/hpc-research/pjones3/rwarburt/Summer2021-rwarburt-2022-01-02/videos/sucroseonly/trimmedvideos") #EDIT
alloutputcsv<-list.files(path = ".", pattern = "\\.csv$", all.files = TRUE,
                         full.names = FALSE, recursive = TRUE,
                         ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
cutend4<- function(arg_1) {
  substr(arg_1,1,nchar(arg_1)-4) 
}
cutend50<- function(arg_1) {
  substr(arg_1,1,nchar(arg_1)-50) 
}

#Create full dataframe to bind everything to 
full<-data.frame(Bee= c(1),
                 Colony= c(1),
                 Treatment= c(1),
                 Bout= c(1),
                 Seconds= c(1),
                 Behavior= c(1),
                 FlowerColor= c(1),
                 Measure= c(1))
#Apply function to list of excel output files, save as new list 
alloutputcsv=lapply(alloutputcsv, cutend4)
#titles list is for calling video out of c2 and naming it at the end
titles=lapply(alloutputcsv, cutend50)
View(alloutputcsv)

#Loop through all DeepLabCut outputs and record when the bee is in the area designated as a flower
for (i in 1:length(alloutputcsv)) { #Edit
  
  setwd("/Volumes/hpc-research/pjones3/rwarburt/Summer2021-rwarburt-2022-01-02/videos/sucroseonly/trimmedvideos") #EDIT for where ever the videos needing to be processed are stored
  Data<-read_csv(paste0(alloutputcsv[i], ".csv")) 
  
  
  
  #Filter Excel Data
  
  Entrance<-Data %>% select(2:4)
  colnames(Entrance) <- c("x",'y',"likelihood")
  Entrance<-Entrance[-1,]
  Entrance<-Entrance[-1,]
  Bee<-Data %>% select (5:7)
  colnames(Bee) <- c("x",'y',"likelihood")
  Bee<-Bee[-1,]
  Bee<-Bee[-1,]
  Bee<-filter(Bee,likelihood>0.5) #EDIT for whatever likelihood threshold desired
  
  #Make them numeric columns, so you can do math on them
  Entrance<-transform(Entrance, x = as.numeric(x))
  Entrance<-transform(Entrance, y = as.numeric(y))
  Entrance<-transform(Entrance, likelihood = as.numeric(likelihood))
  
  Bee<-transform(Bee, x = as.numeric(x))
  Bee<-transform(Bee, y = as.numeric(y))
  Bee<-transform(Bee, likelihood = as.numeric(likelihood))
  
  #Add measure column 
  Bee<-Bee%>% mutate(Measure = 1:n())
  Entrance<-Entrance%>% mutate(Measure = 1:n())
  mean(Entrance$x)
  
  #create new point for every video
  mean(Entrance$x)  #Original Set up used two different arenas with flowers in slightly different positions, so Entrance marker was created to know which video was from which arena
  if (mean(Entrance$x)<1500) {
    #Left Entrance
    ref<-filter(c2, bee==paste0(titles[i])) 
    ref2<- data.frame(t(ref[-1]))
    colnames(ref2) <- c("point1x",'point1y',"point2x", "point2y", "point3x", "point3y")
    rownames(ref2)<-NULL
    ref3<-ref2[2,]
    ref3<-lapply(ref3, as.numeric)
    
    'flower1x'<-ref3$point1x+921.4231303   #These values are from manually calculating the distance between reference points and flowers on a few choice vidoes
    'flower1y'<-ref3$point1y+30.95219818
    'flower2x'<-ref3$point1x+614.2820869
    'flower2y'<-ref3$point1y
    'flower3x'<-ref3$point1x+283.3316602
    'flower3y'<-ref3$point1y
    'flower4x'<-ref3$point1x-30.95219818
    'flower4y'<-ref3$point1y+16.66656825
    'flower5x'<-ref3$point2x+773.80495
    'flower5y'<-ref3$point2y+52.3806
    'flower6x'<-ref3$point2x+407.14095
    'flower6y'<-ref3$point2y+28.5712
    'flower7x'<-ref3$point2x-40.4759
    'flower7y'<-ref3$point2y+26.1903
    'flower8x'<-ref3$point2x-445.23545
    'flower8y'<-ref3$point2y+52.3806
    'flower9x'<-ref3$point3x+426.1879595
    'flower9y'<-ref3$point3y+35.71407482
    'flower10x'<-ref3$point3x-40.47595146
    'flower10y'<-ref3$point3y+45.2378281
    'flower11x'<-ref3$point3x-688.0913
    'flower11y'<-ref3$point3y+54.7616
    'flower12x'<-ref3$point3x-1202.373852
    'flower12y'<-ref3$point3y+40.47595146
    
  } else {
    ref<-filter(c2, bee==paste0(titles[i])) 
    ref2<- data.frame(t(ref[-1]))
    colnames(ref2) <- c("point1x",'point1y',"point2x", "point2y", "point3x", "point3y")
    rownames(ref2)<-NULL
    ref3<-ref2[2,]
    ref3<-lapply(ref3, as.numeric)
    #Right Entrance 
    'flower1x'<-ref3$point1x-911.899377
    'flower1y'<-ref3$point1y-7.142814964
    'flower2x'<-ref3$point1x-595.2345803
    'flower2y'<-ref3$point1y-26.19032153
    'flower3x'<-ref3$point1x-261.9032153
    'flower3y'<-ref3$point1y-21.42844489
    'flower4x'<-ref3$point1x+40.47595146
    'flower4y'<-ref3$point1y+2.380938321
    'flower5x'<-ref3$point2x-809.5190292
    'flower5y'<-ref3$point2y+40.47595146
    'flower6x'<-ref3$point2x-395.2357613
    'flower6y'<-ref3$point2y+11.90468
    'flower7x'<-ref3$point2x+42.85688978
    'flower7y'<-ref3$point2y+16.66656825
    'flower8x'<-ref3$point2x+416.6642062
    'flower8y'<-ref3$point2y+42.85688
    'flower9x'<-ref3$point3x-457.1401577
    'flower9y'<-ref3$point3y+23.80938321
    'flower10x'<-ref3$point3x+54.7617
    'flower10y'<-ref3$point3y+33.33317
    'flower11x'<-ref3$point3x+704.7581
    'flower11y'<-ref3$point3y+30.95217
    'flower12x'<-ref3$point3x+1180.9451
    'flower12y'<-ref3$point3y+11.90467
  }
  
  #Create subset function
  Beevisitflowertop<-function(f_x, f_y) {
    subset(Bee, x > (f_x-37) & x < (f_x+37) & y > (f_y-37) & y < (f_y+37))   #Since the camera is at an angle the flowers at the top appear smaller, since they are farther from the camera
  }                                                                          #In reality, the size of all flowers are equal
  
  Beevisitflowermid<-function(f_x, f_y) {
    subset(Bee, x > (f_x-40) & x < (f_x+40) & y > (f_y-40) & y < (f_y+40))  
  }
  
  Beevisitflowerbottom<-function(f_x, f_y) {
    subset(Bee, x > (f_x-60) & x < (f_x+60) & y > (f_y-60) & y < (f_y+60))
  }
  
  
  #Use subset function to create new dataframes for each visit
  flower1<-Beevisitflowertop(flower1x, flower1y)
  flower2<-Beevisitflowertop(flower2x, flower2y)
  flower3<-Beevisitflowertop(flower3x, flower3y)
  flower4<-Beevisitflowertop(flower4x, flower4y)
  flower5<-Beevisitflowermid(flower5x, flower5y)
  flower6<-Beevisitflowermid(flower6x, flower6y)
  flower7<-Beevisitflowermid(flower7x, flower7y)
  flower8<-Beevisitflowermid(flower8x, flower8y)
  flower9<-Beevisitflowerbottom(flower9x, flower9y)
  flower10<-Beevisitflowerbottom(flower10x, flower10y)
  flower11<-Beevisitflowerbottom(flower11x, flower11y)
  flower12<-Beevisitflowerbottom(flower12x, flower12y)
  
  #Add Visit label this way to avoid errors, inefficient but couldn't figure it out the right way
  if (nrow(flower1)>0) {
    flower1$Visit<- "Flower1Blue"
  }
  
  if (nrow(flower2)>0) {
    flower2$Visit<- "Flower2Yellow"
  }
  
  if (nrow(flower3)>0) {
    flower3$Visit<- "Flower3Blue"
  }
  
  if (nrow(flower4)>0) {
    flower4$Visit<- "Flower4Yellow"
  }
  
  if (nrow(flower5)>0) {
    flower5$Visit<- "Flower5Yellow"
  }
  
  if (nrow(flower6)>0) {
    flower6$Visit<- "Flower6Blue"
  }
  
  if (nrow(flower7)>0) {
    flower7$Visit<- "Flower7Yellow"
  }
  
  if (nrow(flower8)>0) {
    flower8$Visit<- "Flower8Blue"
  }
  if (nrow(flower9)>0) {
    flower9$Visit<- "Flower9Blue"
  }
  
  if (nrow(flower10)>0) {
    flower10$Visit<- "Flower10Yellow"
  }
  
  if (nrow(flower11)>0) {
    flower11$Visit<- "Flower11Blue"
  }
  
  if (nrow(flower12)>0) {
    flower12$Visit<- "Flower12Yellow"
  }

  Flying<-subset(Bee, (x < (flower1x-36) | x > (flower1x+36) | y < (flower1y-36) | y > (flower1y+36)) & #Important for the values here to match the ones in the subset functions above
                   (x < (flower2x-36) | x > (flower2x+36) | y < (flower2y-36) | y > (flower2y+36)) &
                   (x < (flower3x-36) | x > (flower3x+36) | y < (flower3y-36) | y > (flower3y+36)) &
                   (x < (flower4x-36) | x > (flower4x+36) | y < (flower4y-36) | y > (flower4y+36)) &
                   (x < (flower5x-40) | x > (flower5x+40) | y < (flower5y-40) | y > (flower5y+40)) &
                   (x < (flower6x-40) | x > (flower6x+40) | y < (flower6y-40) | y > (flower6y+40)) &
                   (x < (flower7x-40) | x > (flower7x+40) | y < (flower7y-40) | y > (flower7y+40)) &
                   (x < (flower8x-40) | x > (flower8x+40) | y < (flower8y-40) | y > (flower8y+40)) &
                   (x < (flower9x-60) | x > (flower9x+60) | y < (flower9y-60) | y > (flower9y+60)) &
                   (x < (flower10x-60) | x > (flower10x+60) | y < (flower10y-60) | y > (flower10y+60)) &
                   (x < (flower11x-60) | x > (flower11x+60) | y < (flower11y-60) | y > (flower11y+60)) &
                   (x < (flower12x-60) | x > (flower12x+60) | y < (flower12y-60) | y > (flower12y+60)))
  Flying$Visit<-"Flying"
  
  #Create first output
  Beevisits<- bind_rows(flower1, flower2, flower3, flower4, flower5, flower6, flower7, flower8, flower9, flower10, flower11, flower12, Flying)
  
  #Order by measure
  Output<-Beevisits[order(Beevisits$Measure),]
  
  #Rename any visits under 1.5 seconds as flying, as this is indicative of a DeepLabCut error
  Output<-data.frame(unclass(rle(Output$Visit)))
  Output<-Output%>% mutate(values=(
    ifelse(lengths<45 ,"Flying", values)))
  #Divide lengths of visit by framerate to convert to seconds
  Output <- Output %>% 
    mutate(lengths = lengths / 30)
  colnames(Output)<-c("Seconds","Behavior")
  
  
  #Adding Interval
  Interval<-inverse.rle(within.list(rle(Output$Behavior), values <- seq_along(values))) - 1
  Output$Interval=Interval
  
  Outputtemp<-Output
  Outputtemp<-Outputtemp[0,]
  #Getting rid of little errors when a visit is split by label clicking to something else and back
  #      - indicative of deeplabcut lapse
  if (tail(Output$Interval, 1)>1) {
    for (k in 1:(tail(Output$Interval, 1)-1)) {
      current<-subset(Output,Output$Interval==k)
      last<-subset(Output,Output$Interval==k-1)
      next1<-subset(Output,Output$Interval==k+1)
      if (last$Behavior==next1$Behavior && max(current$Seconds)<1.49 && last$Behavior!="Flying") {
        current$Behavior<-last$Behavior
      }
      Outputtemp<-rbind(Outputtemp,current)
    }
  }
  tail(Output$Interval, 1)
  Outputtemp$Seconds<-as.numeric(as.character(Outputtemp$Seconds))

  str(Outputtemp)
  str(Output)
  first<-filter(Output, Output$Interval==0)
  lst<-filter(Output, Output$Interval==max(Output$Interval))
  Outputtemp<-rbind(first,Outputtemp)
  Outputtemp<-rbind(Outputtemp,lst)
  
  Output<-Outputtemp
  #Combining duplicate rows
  Output$M_I <- paste(Output$Behavior,Output$Interval)
  OutputSum <- Output %>%
    group_by(M_I) %>%
    summarise(totfly=sum(Seconds), Int = mean(Interval))
  OutputSum<- OutputSum[order(OutputSum$Int),]
  
  #Organize back to original order and column name
  OutputSum$Int= NULL
  Output2<-OutputSum%>% rename(
    Behavior= M_I,
    Seconds= totfly)
  
  
  #Delete numbers from end of Behaviors
  Output2$Behavior <- gsub("Flower1Blue \\d+", "Flower1Blue", Output2$Behavior)
  Output2$Behavior <- gsub("Flower2Yellow \\d+", "Flower2Yellow", Output2$Behavior)
  Output2$Behavior <- gsub("Flower3Blue \\d+", "Flower3Blue", Output2$Behavior)
  Output2$Behavior <- gsub("Flower4Yellow \\d+", "Flower4Yellow", Output2$Behavior)
  Output2$Behavior <- gsub("Flower5Yellow \\d+", "Flower5Yellow", Output2$Behavior)
  Output2$Behavior <- gsub("Flower6Blue \\d+", "Flower6Blue", Output2$Behavior)
  Output2$Behavior <- gsub("Flower7Yellow \\d+", "Flower7Yellow", Output2$Behavior)
  Output2$Behavior <- gsub("Flower8Blue \\d+", "Flower8Blue", Output2$Behavior)
  Output2$Behavior <- gsub("Flower9Blue \\d+", "Flower9Blue", Output2$Behavior)
  Output2$Behavior <- gsub("Flower10Yellow \\d+", "Flower10Yellow", Output2$Behavior)
  Output2$Behavior <- gsub("Flower11Blue \\d+", "Flower11Blue", Output2$Behavior)
  Output2$Behavior <- gsub("Flower12Yellow \\d+", "Flower12Yellow", Output2$Behavior)
  Output2$Behavior <- gsub("Flying \\d+", "Flying", Output2$Behavior)
  
  #Round second's to better sig figs
  Output2<-Output2 %>% 
    mutate_if(is.numeric, round, digits = 2)
  
  #Getting rid of little errors when a visit is split by less than a second of flying
  #      - indicative of deeplabcut lapse
  output3<-Output2%>% mutate(Behavior=(
    ifelse(Seconds<1.49 & lag(Behavior)==lead(Behavior), lag(Behavior), Output2$Behavior)))
  
  #Getting rid of NAs in last row
  output3[is.na(output3)] = "Flying"
  
  #Adding Interval
  Interval<-inverse.rle(within.list(rle(output3$Behavior), values <- seq_along(values))) - 1
  output3$Interval=Interval
  
  #Combining duplicate rows
  output3$M_I <- paste(output3$Behavior,output3$Interval)
  output3Sum <- output3 %>%
    group_by(M_I) %>%
    summarise(totfly=sum(Seconds), Int = mean(Interval))
  output3Sum<- output3Sum[order(output3Sum$Int),]
  
  #Organize back to original order and column name
  output3Sum$Int= NULL
  output3<-output3Sum%>% rename(
    Behavior= M_I,
    Seconds= totfly)
  
  
  #Delete numbers from end of Behaviors
  output3$Behavior <- gsub("Flower1Blue \\d+", "Flower1Blue", output3$Behavior)
  output3$Behavior <- gsub("Flower2Yellow \\d+", "Flower2Yellow", output3$Behavior)
  output3$Behavior <- gsub("Flower3Blue \\d+", "Flower3Blue", output3$Behavior)
  output3$Behavior <- gsub("Flower4Yellow \\d+", "Flower4Yellow", output3$Behavior)
  output3$Behavior <- gsub("Flower5Yellow \\d+", "Flower5Yellow", output3$Behavior)
  output3$Behavior <- gsub("Flower6Blue \\d+", "Flower6Blue", output3$Behavior)
  output3$Behavior <- gsub("Flower7Yellow \\d+", "Flower7Yellow", output3$Behavior)
  output3$Behavior <- gsub("Flower8Blue \\d+", "Flower8Blue", output3$Behavior)
  output3$Behavior <- gsub("Flower9Blue \\d+", "Flower9Blue", output3$Behavior)
  output3$Behavior <- gsub("Flower10Yellow \\d+", "Flower10Yellow", output3$Behavior)
  output3$Behavior <- gsub("Flower11Blue \\d+", "Flower11Blue", output3$Behavior)
  output3$Behavior <- gsub("Flower12Yellow \\d+", "Flower12Yellow", output3$Behavior)
  output3$Behavior <- gsub("Flying \\d+", "Flying", output3$Behavior)
  #Get the treatment for the current data by finding row with current video
  treatmentlist<-read_csv("C:/Users/Reed/Desktop/Reed's School Work/BeeLab R Projects/videotreatmentcsv.csv")
  v<-which(treatmentlist$Videos == paste0(titles[i]))#EDIT
  
  #Create Variables that have all info
  co<-treatmentlist[v,3]
  buzz<-sub("bout.*", "", treatmentlist[v,1])
  buzz<-substring(buzz, 4)
  tr<-treatmentlist[v,2]
  bout<-sub(".*bout", "", treatmentlist[v,1])
  bout<-sub("\\(.*", "", bout)
  
  #Create all new Label Columns in a weird way but couldn't get it working the right way
  OutputFinal<-output3
  OutputFinal$Bee<-paste0(buzz)
  OutputFinal$Colony<-paste0(co)
  OutputFinal$Treatment<-paste0(tr)
  OutputFinal$Bout<-paste0(bout)
  OutputFinal<-OutputFinal%>% mutate(Measure = 1:n())
  
  #also split Behavior into behavior and color
  OutputFinal$FlowerColor<-"N/A"
  for (j in 1:nrow(OutputFinal)) {
    if (grepl( "Blue", OutputFinal[j,1], fixed = TRUE))
      OutputFinal[j,8]<-"Blue"
    if (grepl( "Yellow", OutputFinal[j,1], fixed = TRUE))
      OutputFinal[j,8]<-"Yellow"
  }
  
  #Delete color from behavior column in a sloppy way
  for (j in 1:nrow(OutputFinal)) {
    if (grepl( "Blue", OutputFinal[j,1], fixed = TRUE))
      OutputFinal[j,1]<-gsub('.{4}$', '', OutputFinal[j,1])
    if (grepl( "Yellow", OutputFinal[j,1], fixed = TRUE))
      OutputFinal[j,1]<-gsub('.{6}$', '', OutputFinal[j,1])
  }
  
  #Reorder columns
  OutputFinal <- OutputFinal[, c(3,4,5,6,2,1,8,7)]
  
  #Get rid of NA in first row
  if (grepl( "NA", OutputFinal[1,6], fixed = TRUE))
    OutputFinal[1,6]<-"Flying"
  
  #create individual dataframe for each csv file 
  assign(paste0(titles[i]), OutputFinal) #Edit
  
  
  #Create one big dataframe of all stuff 
  full<-rbind(full,OutputFinal)
  
} 

backup<-full
full<-full[-1,]

#Save to a place
library(openxlsx)
setwd("/Volumes/hpc-research/pjones3/rwarburt/Summer2021-rwarburt-2022-01-02/videos/sucroseonly/trimmedvideos")
Workbook<-createWorkbook('Summer2021DLCAnalysis')
addWorksheet(Workbook,'Data')
hs<-createStyle(textDecoration = "bold")

writeData(
  Workbook,
  'Data',
  full,
  startCol = 1,
  startRow = 1,
  xy = NULL,
  colNames = TRUE,
  headerStyle = hs)
setColWidths(Workbook,'Data',3, widths = 14)

saveWorkbook(Workbook, "SucroseOnly2021.xlsx")

#If this code doesn't work for you, don't blame me! I study biology and psychology, not computer science.

